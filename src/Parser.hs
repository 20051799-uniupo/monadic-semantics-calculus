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
import Data.Char (isAlphaNum, isLetter)
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
    void $ keyword "using"
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
        <|> try (pPrim @sig)
        <|> (pApp @sig)

pOpCall :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pOpCall = do
    op <- pOp @sig
    args <- parens (pVal @sig `sepBy` symbol ",")
    pure $ Magic op args

pVal :: forall sig. (ParsableSig sig) => Parser (Val sig (EffectSet sig))
pVal = try pNat <|> pBool <|> try (pLam @sig) <|> pVar <|> try (pRecLam @sig) <|> parens (pVal @sig)

pReturn :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pReturn = keyword "return" >> Ret <$> pVal @sig

pDo :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pDo = do
    void $ keyword "do"
    x <- identifier
    void $ symbol "="
    e1 <- pExp @sig
    void $ symbol ";"
    e2 <- pExp @sig
    pure $ Do x e1 e2

pIf :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pIf = do
    void $ keyword "if"
    cond <- pVal @sig
    void $ keyword "then"
    e1 <- pExp @sig
    void $ keyword "else"
    e2 <- pExp @sig
    pure $ If cond e1 e2

pHandle :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pHandle = do
    void $ keyword "handle"
    e <- pExp @sig
    void $ keyword "with"
    h <- pHandler @sig
    pure $ HandleWith e h

pPrim :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pPrim = try pBinary <|> pUnary
  where
    pUnary = do
        op <-
            (keyword "succ" >> pure Suc)
                <|> (keyword "pred" >> pure Pred)
                <|> (keyword "iszero" >> pure IsZero)
        v <- pVal @sig
        pure $ op v

    pBinary = do
        v1 <- pVal @sig
        op <-
            (symbol "+" >> pure Plus)
                <|> (symbol "-" >> pure Minus)
                <|> (symbol "&&" >> pure And)
                <|> (symbol "||" >> pure Or)
        v2 <- pVal @sig
        pure $ op v1 v2

pApp :: forall sig. (ParsableSig sig) => Parser (Exp sig (EffectSet sig))
pApp = App <$> pVal @sig <*> pVal @sig

pVar :: forall sig. Parser (Val sig (EffectSet sig))
pVar = IdentifierVal <$> identifier

pNat :: forall sig. Parser (Val sig (EffectSet sig))
pNat = NatVal . toPeano <$> lexeme L.decimal
    where
    toPeano :: Integer -> Peano
    toPeano 0 = Zero
    toPeano n = Succ $ toPeano $ n - 1


pBool :: forall sig. Parser (Val sig (EffectSet sig))
pBool =
    (keyword "true" >> pure (BoolVal True))
        <|> (keyword "false" >> pure (BoolVal False))

pLam :: forall sig. (ParsableSig sig) => Parser (Val sig (EffectSet sig))
pLam = do
    void $ keyword "lambda" <|> void (symbol "λ")
    x <- identifier
    void $ symbol ":"
    t <- pType @sig
    void $ symbol "."
    body <- pExp @sig
    pure $ LamVal x t body

pRecLam :: forall sig. (ParsableSig sig) => Parser (Val sig (EffectSet sig))
pRecLam = do
    void $ keyword "rec"
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

pType :: forall sig. (ParsableSig sig) => Parser (ValType (EffectSet sig))
pType = do
    t1 <- pAtomType
    ( try
            ( do
                void $ symbol "->"
                eff <- pEffect @sig
                t2 <- pType @sig
                pure $ ArrType (ArrType' (t1, eff, t2))
            )
            <|> pure t1
        )
  where
    pAtomType :: Parser (ValType (EffectSet sig))
    pAtomType =
        parens (pType @sig)
            <|> (keyword "Nat" >> pure NatType)
            <|> (keyword "Bool" >> pure BoolType)

pEffect :: forall sig. (ParsableSig sig) => Parser (EffectSet sig)
pEffect = do
    ops <- braces (pOp @sig `sepBy` symbol ",")
    pure $ EffectSet $ Set.fromList ops

pHandler :: forall sig. (ParsableSig sig) => Parser (Handler sig (EffectSet sig))
pHandler = braces $ do
    clauses <- many (try (pClause @sig <* symbol ","))
    final <- pFinalClause @sig
    pure $ Handler (Map.fromList clauses) final

pClause :: forall sig. (ParsableSig sig) => Parser (sig, Clause sig (EffectSet sig))
pClause = do
    op <- pOp @sig
    params <- parens (identifier `sepBy` symbol ",")
    void $ symbol "->"
    m <- (symbol "c" >> pure Continue) <|> (symbol "s" >> pure Stop)
    body <- pExp @sig
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

keyword :: String -> Parser ()
keyword w = lexeme . try $ do
    _ <- string w
    notFollowedBy (satisfy (\c -> isAlphaNum c || c == '_'))

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    pStart = satisfy (\c -> isLetter c || c == '_')
    pRest = satisfy (\c -> isAlphaNum c || c == '_' || c == '\'')
    p = (:) <$> pStart <*> many pRest
    check x =
        if x `elem` reserved
            then fail $ "Keyword " ++ show x ++ " is reserved"
            else pure x
    reserved =
        [ "do"
        , "return"
        , "if"
        , "then"
        , "else"
        , "true"
        , "false"
        , "handle"
        , "with"
        , "rec"
        , "lambda"
        , "λ"
        ]

instance ParsableSig NDSig where
    pOp = keyword "choose" >> pure Choose

instance ParsableSig (ExceptionSig String) where
    pOp = do
        void $ keyword "raise"
        void $ symbol "<"
        e <- identifier
        void $ symbol ">"
        pure $ Raise e
