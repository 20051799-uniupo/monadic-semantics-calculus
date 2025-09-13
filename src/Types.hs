module Types (ValType (..), EffectType, ExpType (..), ArrType' (..), SubType (..)) where

import Data.Set (Set, empty, intersection, isSubsetOf, union)

newtype ArrType' = ArrType' (ValType, EffectType, ValType)
data ValType = NatType | BoolType | ArrType ArrType' | BotType | TopType

instance Show ValType where
    show x = case x of
        NatType -> "Nat"
        BoolType -> "Bool"
        ArrType (ArrType' (t, e, t')) -> show t ++ " ->" ++ show e ++ " " ++ show t'
        BotType -> "Bot"
        TopType -> "Top"

newtype ExpType = ExpType (ValType, EffectType)

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
    ArrType (ArrType' (t1, e, t2)) <: ArrType (ArrType' (t1', e', t2')) = t1' <: t1 && t2 <: t2' && e <: e'
    BotType <: _ = True
    _ <: TopType = True
    _ <: _ = False

    join t1 t2
        | t1 <: t2 = t2
        | t2 <: t1 = t1
        | ArrType (ArrType' (t1', e', t2')) <- t1
        , ArrType (ArrType' (t1'', e'', t2'')) <- t2 =
            ArrType (ArrType' (t1' `meet` t1'', e' `join` e'', t2' `join` t2''))
        | otherwise = TopType

    meet t1 t2
        | t1 <: t2 = t1
        | t2 <: t1 = t2
        | ArrType (ArrType' (t1', e', t2')) <- t1
        , ArrType (ArrType' (t1'', e'', t2'')) <- t2 =
            ArrType (ArrType' (t1' `join` t1'', e' `meet` e'', t2' `join` t2''))
        | otherwise = BotType

instance SubType ExpType where
    (ExpType (t, e)) <: (ExpType (t', e')) = t <: t' && e <: e'
    join (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `join` t', e `join` e')
    meet (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `join` t', e `meet` e')

class (SubType a, Monoid a) => Effect a

newtype EffectType = EffectSet (Set String)
    deriving (Show, Eq)

instance SubType EffectType where
    (EffectSet e1) <: (EffectSet e2) = e1 `isSubsetOf` e2
    join (EffectSet e1) (EffectSet e2) = EffectSet $ e1 `union` e2
    meet (EffectSet e1) (EffectSet e2) = EffectSet $ e1 `intersection` e2

instance Semigroup EffectType where
    (EffectSet s1) <> (EffectSet s2) = EffectSet (s1 `union` s2)

instance Monoid EffectType where
    mempty = EffectSet empty
    mappend = (<>)

instance Effect EffectType
