module Parser (pWhole, ParsedProgram (..)) where

import Control.Monad (void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy
import Data.Set qualified as Set
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Core (Res)
import Effects.Exceptions (ExceptionSig (..))
import Effects.Nondeterminism (NDSig (..))
import Language
import Nat (Peano (..))
import Types

data Semantics
    = forall m sig.
        (MonSem m sig, ParsableSig sig, Show (m (Res (Val sig (EffectSet sig))))) =>
      Semantics (Proxy m) (Proxy sig)

semanticsRegistry :: Map (String, String) Semantics
semanticsRegistry =
    Map.fromList
        [ (("Nondeterminism", "List"), Semantics (Proxy @[]) (Proxy @NDSig))
        , (("Exceptions", "Exceptions"), Semantics (Proxy @(Either String)) (Proxy @(ExceptionSig String)))
        ]

class (Sig sig, Ord sig, Show sig) => ParsableSig sig where
    pOp :: Parser sig

data ParsedProgram
    = forall m sig.
        ( MonSem m sig
        , ParsableSig sig
        , Show (m (Res (Val sig (EffectSet sig))))
        ) =>
      ParsedProgram (Proxy m) (Exp sig (EffectSet sig))

deriving instance Show ParsedProgram

pWhole :: Parser ParsedProgram
pWhole = do
    sc
    void $ symbol "using"
    sigName <- identifier
    void $ symbol "@"
    mName <- identifier

    case Map.lookup (sigName, mName) semanticsRegistry of
        Just (Semantics (pm :: Proxy m) (_ps :: Proxy sig)) -> do
            ast <- braces (pExp @sig)
            eof
            pure (ParsedProgram pm ast)
        Nothing ->
            fail $ "No semantics for " ++ sigName ++ " under " ++ mName ++ "."

pExp :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pExp =
    try (pDo @sig)
        <|> try (pIf @sig)
        <|> try (pHandle @sig)
        <|> try (pReturn @sig)
        <|> try (pOpCall @sig)
        <|> (pApp @sig)

pOpCall :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pOpCall = do
    op <- pOp @sig
    args <- parens (pVal @sig `sepBy` symbol ",")
    pure $ Magic op args

pVal :: forall sig. (ParsableSig sig) => Parser (Val sig (EffectSet sig))
pVal = try pNat <|> pBool <|> pVar <|> (pLam @sig) <|> (pRecLam @sig) <|> parens (pVal @sig)

pReturn :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pReturn = symbol "return" >> Ret <$> pVal @sig

pDo :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pDo = do
    void $ symbol "do"
    x <- identifier
    void $ symbol "="
    e1 <- pExp @sig
    void $ symbol ";"
    e2 <- pExp @sig
    pure $ Do x e1 e2

pIf :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pIf = do
    void $ symbol "if"
    cond <- pVal @sig
    void $ symbol "then"
    e1 <- pExp @sig
    void $ symbol "else"
    e2 <- pExp @sig
    pure $ If cond e1 e2

pHandle :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pHandle = do
    void $ symbol "handle"
    e <- pExp @sig
    void $ symbol "with"
    h <- pHandler @sig
    pure $ HandleWith e h

pApp :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pApp = App <$> pVal @sig <*> pVal @sig

pVar :: forall sig. Parser (Val sig (EffectSet sig))
pVar = IdentifierVal <$> identifier

pNat :: forall sig. Parser (Val sig (EffectSet sig))
pNat = NatVal . toPeano <$> L.decimal

pBool :: forall sig. Parser (Val sig (EffectSet sig))
pBool =
    (symbol "true" >> pure (BoolVal True))
        <|> (symbol "false" >> pure (BoolVal False))

pLam :: forall sig. (ParsableSig sig) => Parser (Val sig (EffectSet sig))
pLam = do
    void $ symbol "lambda" <|> symbol "λ"
    x <- identifier
    void $ symbol ":"
    t <- pType @sig
    void $ symbol "."
    body <- pExp @sig
    pure $ LamVal x t body

pRecLam :: forall sig. (ParsableSig sig) => Parser (Val sig (EffectSet sig))
pRecLam = do
    void $ symbol "rec"
    f <- identifier
    void $ symbol ":"
    fullType <- pType @sig
    case fullType of
        ArrType at -> do
            void $ symbol "."
            x <- identifier
            void $ symbol "."
            body <- pExp @sig
            pure $ RecLamVal f at x body
        _ -> fail "Recursive functions must have function type."

-- TODO: fix halt due to infinite recursion with invalid type
pType :: forall sig. (ParsableSig sig) => Parser (ValType (EffectSet sig))
pType =
    (symbol "Nat" >> pure NatType)
        <|> (symbol "Bool" >> pure BoolType)
        <|> (ArrType <$> pArrType @sig)
        <|> parens (pType @sig)

pArrType :: forall sig. (ParsableSig sig) => Parser (ArrType' (EffectSet sig))
pArrType = do
    t1 <- pType @sig
    void $ symbol "->"
    eff <- pEffect @sig
    t2 <- pType @sig
    pure $ ArrType' (t1, eff, t2)

pEffect :: forall sig. (ParsableSig sig) => Parser (EffectSet sig)
pEffect = do
    ops <- braces (pOp @sig `sepBy` symbol ",")
    pure $ EffectSet $ Set.fromList ops

pHandler :: forall sig. (ParsableSig sig) => Parser (Handler sig (EffectSet sig))
pHandler = braces $ do
    clauses <- many (try (pClause @sig))
    final <- pFinalClause @sig
    pure $ Handler (Map.fromList clauses) final

pClause :: forall sig. (ParsableSig sig) => Parser (sig, Clause sig (EffectSet sig))
pClause = do
    op <- pOp @sig
    params <- parens (identifier `sepBy` symbol ",")
    void $ symbol "->"
    m <- (symbol "c" >> pure Continue) <|> (symbol "s" >> pure Stop)
    body <- pExp @sig
    void $ symbol ","
    pure (op, Clause m params body)

pFinalClause :: forall sig. (ParsableSig sig) => Parser (Identifier, Exp sig (EffectSet sig))
pFinalClause = do
    x <- identifier
    void $ symbol "->"
    body <- pExp @sig
    pure (x, body)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reserved then fail $ "keyword " ++ show x ++ " is reserved" else pure x
    reserved =
        [ "semantics"
        , "in"
        , "do"
        , "return"
        , "if"
        , "then"
        , "else"
        , "true"
        , "false"
        , "handle"
        , "with"
        , "rec"
        , "Nat"
        , "Bool"
        , "resume"
        , "stop"
        , "lambda"
        ]

toPeano :: Int -> Peano
toPeano 0 = Zero
toPeano n = Succ (toPeano (n - 1))

instance ParsableSig NDSig where
    pOp = symbol "choose" >> pure Choose

instance ParsableSig (ExceptionSig String) where
    pOp = do
        void $ symbol "raise"
        void $ symbol "<"
        e <- identifier
        void $ symbol ">"
        pure $ Raise e
