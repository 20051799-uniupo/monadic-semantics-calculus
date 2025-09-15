module Types (ValType (..), EffectSet(..), ExpType (..), ArrType' (..), PartialOrd(..), Lattice(..), Effect) where

import Data.Set (Set, empty, intersection, isSubsetOf, union)

class PartialOrd a where
    (<:) :: a -> a -> Bool
    infix 4 <:

class PartialOrd a => Lattice a where
    join :: a -> a -> a
    meet :: a -> a -> a

newtype ArrType' e = ArrType' (ValType e, e, ValType e)
data ValType e = NatType | BoolType | ArrType (ArrType' e) | BotType | TopType

instance (PartialOrd e) => PartialOrd (ValType e) where
    NatType <: NatType = True
    BoolType <: BoolType = True
    ArrType (ArrType' (t1, e, t2)) <: ArrType (ArrType' (t1', e', t2')) = t1' <: t1 && t2 <: t2' && e <: e'
    BotType <: _ = True
    _ <: TopType = True
    _ <: _ = False

instance (Lattice e) => Lattice (ValType e) where
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
            ArrType (ArrType' (t1' `join` t1'', e' `meet` e'', t2' `meet` t2''))
        | otherwise = BotType

instance (Show e) => Show (ValType e) where
    show x = case x of
        NatType -> "Nat"
        BoolType -> "Bool"
        ArrType (ArrType' (t, e, t')) -> show t ++ " ->" ++ show e ++ " " ++ show t'
        BotType -> "Bot"
        TopType -> "Top"

newtype ExpType e = ExpType ((ValType e), e)

instance (PartialOrd e) => PartialOrd (ExpType e) where
    (ExpType (t, e)) <: (ExpType (t', e')) = t <: t' && e <: e'

instance (Lattice e) => Lattice (ExpType e) where
    join (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `join` t', e `join` e')
    meet (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `meet` t', e `meet` e')

instance (Show e) => Show (ExpType e) where
    show (ExpType (t, e)) = show t ++ "!" ++ show e

newtype EffectSet s = EffectSet (Set s)
    deriving (Show, Eq)

class (Lattice e, Monoid e) => Effect e

instance (Ord s) => PartialOrd (EffectSet s) where
    (EffectSet e1) <: (EffectSet e2) = e1 `isSubsetOf` e2

instance (Ord s) => Lattice (EffectSet s) where
    join (EffectSet e1) (EffectSet e2) = EffectSet $ e1 `union` e2
    meet (EffectSet e1) (EffectSet e2) = EffectSet $ e1 `intersection` e2

instance (Ord s) => Semigroup (EffectSet s) where
    (EffectSet s1) <> (EffectSet s2) = EffectSet (s1 `union` s2)

instance (Ord s) => Monoid (EffectSet s) where
    mempty = EffectSet empty
    mappend = (<>)

instance (Ord s) => Effect (EffectSet s)
