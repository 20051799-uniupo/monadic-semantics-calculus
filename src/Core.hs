{-# LANGUAGE MultiParamTypeClasses #-}

module Core
  ( Sig (..),
    MonSem (..),
    Res(..),
    Conf(..),
    reduceStep,
  )
where

import Language

class Sig s where
  arity :: s -> Int

class (Monad m, Sig sig) => MonSem m sig where
  run :: sig -> [Val sig] -> m (Val sig)

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

data Res sig = Ok (Val sig) | Wr deriving (Show)

data Conf sig = ExpConf (Exp sig) | ResConf (Res sig) deriving (Show)

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
