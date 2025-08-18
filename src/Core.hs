{-# LANGUAGE MultiParamTypeClasses #-}

module Core
  ( Res (..),
    Conf (..),
    reduceStep,
    eval,
  )
where

import Language

data Res sig = Ok (Val sig) | Wr deriving (Show)

data Conf sig = ExpConf (Exp sig) | ResConf (Res sig)

instance Show sig => Show (Conf sig) where
    show (ExpConf e) = show e
    show (ResConf r) = show r

reduceStep :: (MonSem m sig, Ord sig) => Conf sig -> m (Conf sig)
reduceStep c = case c of
  ResConf _ -> pure c -- RES
  ExpConf e ->
    case e of
      Ret v -> pure (ResConf (Ok v)) -- RET
      _ ->
        case reduce e of
          Just m_e' -> ExpConf <$> m_e' -- EXP
          Nothing -> pure (ResConf Wr) -- WRONG

eval :: (MonSem m sig, Ord sig) => Conf sig -> m (Res sig)
eval e = evalLoop $ pure e

evalLoop :: (MonSem m sig, Ord sig) => m (Conf sig) -> m (Res sig)
evalLoop m_conf = do
  conf <- m_conf
  case conf of
    ResConf r -> pure r
    ExpConf _ -> evalLoop (reduceStep conf)
