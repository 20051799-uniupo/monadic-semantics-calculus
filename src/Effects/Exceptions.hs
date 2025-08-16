{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Exceptions (
    ExceptionSig(..)
) where

import Lib

data ExceptionSig e = Throw e

instance Sig (ExceptionSig e) where
    arity (Throw _) = 0

instance MonSem (Either e) (ExceptionSig e)  where
    run (Throw e) [] = Left e
    run _ _ = undefined
