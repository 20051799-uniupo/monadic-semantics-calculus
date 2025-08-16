{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Nondeterminism (
    NDSig(..)
) where

import Lib

data NDSig = Choose

instance Sig NDSig where
  arity Choose = 0

instance MonSem [] NDSig where
  run Choose [] = [BoolVal True, BoolVal False]
  run Choose _ = undefined
