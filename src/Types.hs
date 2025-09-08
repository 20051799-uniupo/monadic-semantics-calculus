module Types (ValType (..), Effect, ExpType (..), ArrType' (..), SubType (..)) where

newtype ArrType' = ArrType' (ValType, Effect, ValType)
data ValType = NatType | BoolType | ArrType ArrType' | BotType

instance Show ValType where
    show x = case x of
        NatType -> "Nat"
        BoolType -> "Bool"
        ArrType (ArrType' (t, e, t')) -> show t ++ " ->" ++ show e ++ " " ++ show t'
        BotType -> "Bot"

newtype ExpType = ExpType (ValType, Effect)

instance Show ExpType where
    show (ExpType (t, e)) = show t ++ "!" ++ show e

class SubType a where
    (<:) :: a -> a -> Bool
    infix 4 <:

instance SubType ValType where
    NatType <: NatType = True
    BoolType <: BoolType = True
    ArrType (ArrType' (t1, e, t2)) <: ArrType (ArrType' (t1', e', t2')) = t1' <: t1 && t2 <: t2' && e <= e'
    BotType <: _ = True
    _ <: _ = False

instance SubType ExpType where
    (ExpType (t, e)) <: (ExpType (t', e')) = t <: t' && e <= e'

-- `Eq`, `Ord` already implemented
type Effect = ()
