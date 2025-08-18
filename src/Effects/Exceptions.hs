{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Exceptions
  ( ExceptionSig (..),
  )
where

import Language

data ExceptionSig e = Raise e deriving (Show, Eq, Ord)

instance Sig (ExceptionSig e) where
  arity (Raise _) = 0

instance MonSem (Either e) (ExceptionSig e) where
  run (Raise e) [] = Left e
  run _ _ = undefined
