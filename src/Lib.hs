{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
  ( Identifier,
    Val (..),
    Exp (..),
    Sig (..),
    MonSem (..),
    subst,
    substVal,
    -- reduce,
  )
where

type Identifier = String

class Sig s where
  arity :: s -> Int

data Peano = Zero | Succ Peano deriving (Show)

add :: Peano -> Peano -> Peano
add Zero n = n
add (Succ m) n = Succ (add m n)

sub :: Peano -> Peano -> Peano
sub _ Zero = Zero
sub Zero _ = Zero
sub (Succ m) (Succ n) = sub m n

data Val sig
  = NumVal Peano
  | BoolVal Bool
  | LamVal Identifier (Exp sig)
  | RecLamVal Identifier Identifier (Exp sig)
  | IdentifierVal Identifier
  deriving (Show)

data Exp sig
  = App (Val sig) (Val sig)
  | Ret (Val sig)
  | Do Identifier (Exp sig) (Exp sig)
  | Magic sig [Val sig]
  | If (Val sig) (Exp sig) (Exp sig)
  | Plus (Val sig) (Val sig)
  | Minus (Val sig) (Val sig)
  | And (Val sig) (Val sig)
  | Or (Val sig) (Val sig)
  | IsZero (Val sig)
  deriving (Show)

class (Monad m, Sig sig) => MonSem m sig where
  run :: sig -> [Val sig] -> m (Val sig)

subst :: Identifier -> Val sig -> Exp sig -> Exp sig
subst varName val e = case e of
  App v1 v2 -> App (substVal varName val v1) (substVal varName val v2)
  Ret v -> Ret (substVal varName val v)
  Do x e1 e2 ->
    if x == varName
      then Do x (subst varName val e1) e2
      else Do x (subst varName val e1) (subst varName val e2)
  Magic s vs -> Magic s (substVal varName val <$> vs)
  If p bThen bElse -> If (substVal varName val p) (subst varName val bThen) (subst varName val bElse)
  Plus (a) (b) -> Plus (substVal varName val a) (substVal varName val b)
  Minus (a) (b) -> Minus (substVal varName val a) (substVal varName val b)
  And (a) (b) -> And (substVal varName val a) (substVal varName val b)
  Or (a) (b) -> Or (substVal varName val a) (substVal varName val b)
  IsZero (n) -> IsZero (substVal varName val n)

substVal :: Identifier -> Val sig -> Val sig -> Val sig
substVal varName val (IdentifierVal x) | x == varName = val
substVal varName val (LamVal x body)
  | x == varName = LamVal x body
  | otherwise = LamVal x (subst varName val body)
substVal varName val v@(RecLamVal f x body)
  | f == varName || x == varName = v
  | otherwise = RecLamVal f x (subst varName val body)
substVal _ _ v = v

reduce :: (MonSem m sig) => Exp sig -> Maybe (m (Exp sig))
reduce e = case e of
  Magic op vs -> Just $ Ret <$> run op vs -- EFFECT
  Do var (Ret val) e2 -> Just $ pure $ subst var val e2 -- RET
  Do var e1 e2 ->
    -- DO
    case reduce e1 of
      Just m_e1' -> Just $ (\e1' -> Do var e1' e2) <$> m_e1'
      Nothing -> Nothing
  App (LamVal var body) arg -> Just $ pure $ subst var arg body -- PURE
  App v@(RecLamVal f x body) arg -> Just $ pure $ (subst x arg . subst f v) body -- PURE
  If (BoolVal True) bThen _ -> Just $ pure bThen -- PURE
  If (BoolVal False) _ bElse -> Just $ pure bElse -- PURE
  Plus (NumVal a) (NumVal b) -> Just $ pure $ Ret $ NumVal $ add a b -- PURE
  Minus (NumVal a) (NumVal b) -> Just $ pure $ Ret $ NumVal $ sub a b -- PURE
  And (BoolVal True) (BoolVal True) -> Just $ pure $ Ret $ BoolVal True -- PURE
  And (BoolVal _) (BoolVal _) -> Just $ pure $ Ret $ BoolVal False -- PURE
  Or (BoolVal False) (BoolVal False) -> Just $ pure $ Ret $ BoolVal False -- PURE
  Or (_) (_) -> Just $ pure $ Ret $ BoolVal True -- PURE
  IsZero (NumVal Zero) -> Just $ pure $ Ret $ BoolVal True -- PURE
  IsZero (NumVal _) -> Just $ pure $ Ret $ BoolVal False -- PURE
  _ -> Nothing

data Res sig = Ok (Val sig) | Wr

data Conf sig = ExpConf (Exp sig) | ResConf (Res sig)

reduceStep :: (MonSem m sig) => Conf sig -> m (Conf sig)
reduceStep c = case c of
  ResConf _ -> pure c -- RES
  ExpConf e ->
    case e of
      Ret v -> pure (ResConf (Ok v)) -- RET
      _ ->
        case reduce e of
          Just m_e' -> ExpConf <$> m_e' -- EXP
          Nothing -> pure (ResConf Wr) -- WRONG
