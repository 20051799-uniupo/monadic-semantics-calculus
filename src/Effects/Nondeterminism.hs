{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Nondeterminism (
    NDSig (..),
)
where

import Language
import Types (ValType (BoolType))

data NDSig = Choose deriving (Show, Eq, Ord)

instance Sig NDSig where
    arity Choose = ([], BoolType)

instance MonSem [] NDSig where
    run Choose [] = [BoolVal True, BoolVal False]
    run Choose _ = undefined
