{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( 
    Identifier,
    Val (..),
    Exp (..),
    Sig (..),
    MonSem (..),
    subst,
    substVal,
    reduce,
  )
where

type Identifier = String

class Sig s where
  arity :: s -> Int

data Val sig = Tru | Fls | Lam Identifier (Exp sig) | RecLam Identifier Identifier (Exp sig) | Vr Identifier

data Exp sig
  = App (Val sig) (Val sig)
  | Ret (Val sig)
  | Do Identifier (Exp sig) (Exp sig)
  | Magic sig [Val sig]
  | If (Val sig) (Exp sig) (Exp sig)

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

substVal :: Identifier -> Val sig -> Val sig -> Val sig
substVal varName val (Vr x) | x == varName = val
substVal varName val (Lam x body)
  | x == varName = Lam x body
  | otherwise = Lam x (subst varName val body)
substVal varName val v@(RecLam f x body)
  | f == varName || x == varName = v
  | otherwise = RecLam f x (subst varName val body)
substVal _ _ v = v

reduce :: (MonSem m sig) => Exp sig -> m (Exp sig)
reduce e = case e of
  Magic op vs -> Ret <$> run op vs -- EFFECT
  Do var (Ret val) e2 -> pure $ subst var val e2 -- RET
  Do var e1 e2 -> Do var <$> reduce e1 <*> pure e2 -- DO
  App (Lam var body) arg -> pure $ subst var arg body -- PURE
  App v@(RecLam f x body) arg -> pure $ (subst x arg . subst f v) body -- PURE
  -- PURE
  If p bThen bElse ->
    pure
      ( case p of
          Tru -> bThen
          Fls -> bElse
      )
