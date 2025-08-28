module Types (ValType(..), EffectType, ExpType) where

data ValType = NatType | BoolType | ArrType ValType EffectType ValType | TypeVar Int deriving (Show, Eq)
type EffectType = ()
type ExpType = (ValType, EffectType)
