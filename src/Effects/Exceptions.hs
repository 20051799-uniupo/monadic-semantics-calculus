{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Exceptions
  ( ExceptionSig (..),
  )
where

import Core

data ExceptionSig e = Raise e deriving (Show)

instance Sig (ExceptionSig e) where
  arity (Raise _) = 0

instance MonSem (Either e) (ExceptionSig e) where
  run (Raise e) [] = Left e
  run _ _ = undefined
