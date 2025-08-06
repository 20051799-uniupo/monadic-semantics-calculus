{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Nondeterminism (
    NDSig(..)
) where

import Lib

data NDSig = Choose
    deriving (Show, Eq)

instance Sig NDSig where
  arity Choose = 0

instance MonSem [] NDSig where
  run Choose _ = [Tru, Fls]
