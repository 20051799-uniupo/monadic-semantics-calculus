module Types (ValType (..), Effect, ExpType (..), ArrType' (..), SubType (..)) where

newtype ArrType' = ArrType' (ValType, Effect, ValType)
data ValType = NatType | BoolType | ArrType ArrType' | BotType | TopType

instance Show ValType where
    show x = case x of
        NatType -> "Nat"
        BoolType -> "Bool"
        ArrType (ArrType' (t, e, t')) -> show t ++ " ->" ++ show e ++ " " ++ show t'
        BotType -> "Bot"
        TopType -> "Top"

newtype ExpType = ExpType (ValType, Effect)

instance Show ExpType where
    show (ExpType (t, e)) = show t ++ "!" ++ show e

class SubType a where
    (<:) :: a -> a -> Bool
    infix 4 <:

    join :: a -> a -> a
    meet :: a -> a -> a

instance SubType ValType where
    NatType <: NatType = True
    BoolType <: BoolType = True
    ArrType (ArrType' (t1, e, t2)) <: ArrType (ArrType' (t1', e', t2')) = t1' <: t1 && t2 <: t2' && e <= e'
    BotType <: _ = True
    _ <: _ = False

    join t1 t2
        | t1 <: t2 = t2
        | t2 <: t1 = t1
        | ArrType (ArrType' (t1', e', t2')) <- t1, ArrType (ArrType' (t1'', e'', t2'')) <- t2 = do
            ArrType (ArrType' (t1' `meet` t1'', max e' e'', t2' `join` t2''))
        | otherwise = TopType

    meet t1 t2
        | t1 <: t2 = t1
        | t2 <: t1 = t2
        | ArrType (ArrType' (t1', e', t2')) <- t1, ArrType (ArrType' (t1'', e'', t2'')) <- t2 = do
            ArrType (ArrType' (t1' `join` t1'', min e' e'', t2' `join` t2''))
        | otherwise = BotType

instance SubType ExpType where
    (ExpType (t, e)) <: (ExpType (t', e')) = t <: t' && e <= e'
    join (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `join` t', max e e')
    meet (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `join` t', min e e')

-- `Eq`, `Ord` already implemented
type Effect = ()
