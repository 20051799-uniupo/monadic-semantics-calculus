{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Core
  ( Res (..),
    Conf (..),
    Valuable(..),
    Reducible(..),
    reduceStep,
    reduceMnStep,
    evalLoop,
    evalFin
  )
where

class Valuable e v | e -> v where
    toVal :: e -> Maybe v

class (Monad m, Valuable e v) => Reducible m e v where
    reduce :: e -> Maybe (m e)

data Res v = Ok v | Wr deriving (Show)
data Conf e v = ExpConf e | ResConf (Res v) deriving (Show)

reduceStep :: (Reducible m e v) => Conf e v -> m (Conf e v)
reduceStep c = case c of
  ResConf _ -> pure c -- RES
  ExpConf e ->
    case toVal e of
      Just v -> pure (ResConf (Ok v)) -- RET
      Nothing -> case reduce e of
          Just m_e' -> ExpConf <$> m_e' -- EXP
          Nothing -> pure (ResConf Wr) -- WRONG

reduceMnStep :: (Reducible m e v) => m (Conf e v) -> m (Conf e v)
reduceMnStep c = c >>= reduceStep

evalLoop :: (Reducible m e v) => m (Conf e v) -> m (Res v)
evalLoop m_conf = do
  conf <- m_conf
  case conf of
    ResConf r -> pure r
    ExpConf _ -> evalLoop (reduceStep conf)

evalFin :: (Reducible m e v) => e -> m (Res v)
evalFin e = evalLoop $ pure $ ExpConf e
