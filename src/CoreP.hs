{-# LANGUAGE MultiParamTypeClasses #-}

module Core
  ( Res (..),
    Conf (..),
    reduceStep,
    evalFin,
  )
where

import Language

data Res sig = Ok (Val sig) | Wr deriving (Show)

data Conf sig = ExpConf (Exp sig) | ResConf (Res sig) deriving (Show)


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

reduceMnStep :: (MonSem m sig, Ord sig) => m (Conf sig) -> m (Conf sig)
reduceMnStep c = c >>= reduceStep

evalFin :: (MonSem m sig, Ord sig) => Exp sig -> m (Res sig)
evalFin e = evalLoop (pure (ExpConf e))

evalLoop :: (MonSem m sig, Ord sig) => m (Conf sig) -> m (Res sig)
evalLoop m_conf = do
  conf <-  m_conf
  case conf of
    (ResConf r) -> pure r
    _ -> evalLoop (reduceMnStep m_conf)
