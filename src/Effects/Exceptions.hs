{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Exceptions
  ( ExceptionSig (..),
  )
where

import Language
import Types(ValType(NatType))

data ExceptionSig e = Raise e deriving (Show, Eq, Ord)

instance Sig (ExceptionSig e) where
    -- FIXME: `NatType` is wrong, should instead introduce `ValType(Bot)` but needs proper subtyping
    signature (Raise _) = ([], NatType)

instance MonSem (Either e) (ExceptionSig e) where
  run (Raise e) [] = Left e
  run _ _ = undefined
