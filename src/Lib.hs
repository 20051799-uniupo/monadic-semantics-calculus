{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib (
    Var,
    Val(..),
    Exp(..),
    
    Sig(..),
    MonSem(..),

    subst,
    substVal,
    reduce
) where

type Var = String

class Sig s where
    arity :: s -> Int

data Val sig = I Int | Tru | Fls | Lam Var (Exp sig) | V Var

data Exp sig = App (Val sig) (Val sig)
             | Ret (Val sig)
             | Do Var (Exp sig) (Exp sig)
             | Magic sig [Val sig]
             | If (Val sig) (Exp sig) (Exp sig)

class (Monad m, Sig sig) => MonSem m sig where
    run :: sig -> [Val sig] -> m (Val sig)

subst :: Var -> Val sig -> Exp sig -> Exp sig
subst varName val (App v1 v2)   = App (substVal varName val v1) (substVal varName val v2)
subst varName val (Ret v)       = Ret (substVal varName val v)
subst varName val (Do x e1 e2)  = if x == varName
                                  then Do x (subst varName val e1) e2
                                  else Do x (subst varName val e1) (subst varName val e2)
subst varName val (Magic s vs)  = Magic s (substVal varName val <$> vs)
subst varName val (If p bThen bElse) = If (substVal varName val p) (subst varName val bThen) (subst varName val bElse)

substVal :: Var -> Val sig -> Val sig -> Val sig
substVal varName val (V x)        | x == varName = val
substVal varName _ (Lam x body) | x == varName = Lam x body
substVal varName val (Lam x body) = Lam x (subst varName val body)
substVal _ _ v = v

reduce :: MonSem m sig => Exp sig -> m (Exp sig)
reduce (Magic op vs)          = Ret <$> run op vs -- EFFECT
reduce (Do var (Ret val) e2)  = pure $ subst var val e2 -- RET
reduce (Do var e1 e2)         = Do var <$> reduce e1 <*> pure e2 -- DO
reduce e@(Ret _)              = pure e -- PURE
reduce (App (Lam var body) arg) = pure $ subst var arg body -- APP + PURE

reduce (If p bThen bElse) = case p of
    Tru -> reduce bThen
    Fls -> reduce bElse
