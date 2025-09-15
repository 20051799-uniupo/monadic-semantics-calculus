{-# LANGUAGE MultiParamTypeClasses #-}

module Core
  ( Res (..),
    Conf (..),
    reduceStep,
    reduceMnStep,
    evalFin,
  )
where

import Language

data Res sig e = Ok (Val sig e) | Wr deriving (Show)

data Conf sig e = ExpConf (Exp sig e) | ResConf (Res sig e) deriving (Show)

reduceStep :: (MonSem m sig, Ord sig) => Conf sig e -> m (Conf sig e)
reduceStep c = case c of
  ResConf _ -> pure c -- RES
  ExpConf e ->
    case e of
      Ret v -> pure (ResConf (Ok v)) -- RET
      _ ->
        case reduce e of
          Just m_e' -> ExpConf <$> m_e' -- EXP
          Nothing -> pure (ResConf Wr) -- WRONG

reduceMnStep :: (MonSem m sig, Ord sig) => m (Conf sig e) -> m (Conf sig e)
reduceMnStep c = c >>= reduceStep

evalFin :: (MonSem m sig, Ord sig) => Exp sig e -> m (Res sig e)
evalFin e = evalLoop $ pure $ ExpConf e

evalLoop :: (MonSem m sig, Ord sig) => m (Conf sig e) -> m (Res sig e)
evalLoop m_conf = do
  conf <- m_conf
  case conf of
    ResConf r -> pure r
    ExpConf _ -> evalLoop (reduceStep conf)
