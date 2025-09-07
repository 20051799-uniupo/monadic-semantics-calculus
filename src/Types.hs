module Types (ValType(..), Effect, ExpType) where

data ValType = NatType | BoolType | ArrType ValType Effect ValType | TypeVar Int deriving (Show, Eq)
type ExpType = (ValType, Effect)
type Effect = ()
