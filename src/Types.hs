{- |
Module      : Types
Description : Type definitions and effect algebra.

This module defines the data structures for types and effects, and the
algebraic rules used for subtyping and subeffecting and effect handling.
-}
module Types (ValType (..), EffectSet (..), ExpType (..), ArrType' (..), PartialOrd (..), Lattice (..), Effect (..), Mode (..), ClauseFilter (..), Filter (..)) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, intersection, isSubsetOf, singleton, union)
import Data.Set qualified as Set

{- | Abstract class for types equipped with a partial order relation.

Represents a poset \\( \\langle A \\preceq \\rangle \\).
-}
class PartialOrd a where
    {- | The partial order relation \\( \\preceq \\).

    Returns 'True' if the first argument is smaller than or equal to the second.
    -}
    (<:) :: a -> a -> Bool

    infix 4 <:

{- | Abstract class for types forming a lattice.

Represents a lattice \\( \\langle A, \\vee, \\wedge \\rangle \\).
-}
class (PartialOrd a) => Lattice a where
    -- | The join operation (least upper bound), denoted \\( \\vee \\).
    join :: a -> a -> a

    -- | The meet operation (greatest lower bound), denoted \\( \\wedge \\).
    meet :: a -> a -> a

{- | Helper newtype for function types.

Represents the triple \\( (T_1, E, T_2) \\) in a function type \\( T_1 \\xrightarrow{E} T_2 \\).
-}
newtype ArrType' e = ArrType' (ValType e, e, ValType e)

{- | The set of value types \\( T \\).

Defined inductively as:

\\[
T ::= \\mathsf{Nat} \\mid \\mathsf{Bool} \\mid T \\xrightarrow{E} T' \\mid \\bot \\mid \\top
\\]

where \\( E \\) is an effect type.
-}
data ValType e
    = -- | The type of natural numbers (\\( \\mathsf{Nat} \\)).
      NatType
    | -- | The type of booleans (\\( \\mathsf{Bool} \\)).
      BoolType
    | -- | The function type \\( T \\xrightarrow{E} T' \\).
      ArrType (ArrType' e)
    | -- | The bottom type (\\( \\bot \\)), subtype of all types.
      BotType
    | -- | The top type (\\( \\top \\)), supertype of all types.
      TopType

{- | Defines the subtyping relation on value types.

Includes the standard covariant/contravariant rule for function types:

\\[
T_1 \\xrightarrow{E} T_2 \\le T'_1 \\xrightarrow{E'} T'_2 \\iff T'_1 \\le T_1 \\land T_2 \\le T'_2 \\land E \\le E'
\\]
-}
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

{- | The type of an expression, annotated with an effect.

Represents the pair \\( T ! E \\), asserting that an expression returns a value of type \\( T \\)
and produces effects approximated by \\( E \\).
-}
newtype ExpType e = ExpType ((ValType e), e)

{- | Subtyping for expression types.

\\[
T ! E \\le T' ! E' \\iff T \\le T' \\land E \\le E'
\\]
-}
instance (PartialOrd e) => PartialOrd (ExpType e) where
    (ExpType (t, e)) <: (ExpType (t', e')) = t <: t' && e <: e'

instance (Lattice e) => Lattice (ExpType e) where
    join (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `join` t', e `join` e')
    meet (ExpType (t, e)) (ExpType (t', e')) = ExpType (t `meet` t', e `meet` e')

instance (Show e) => Show (ExpType e) where
    show (ExpType (t, e)) = show t ++ "!" ++ show e

-- | Evaluation mode for handlers.
data Mode
    = -- | \( \mathsf{c} \):  Continue execution
      Continue
    | -- | \( \mathsf{s} \): Stop execution
      Stop
    deriving (Eq)

{- | Describes how a specific operation is handled.

Corresponds to the target of a \\( op(\\overline{x}) \\to_\\mu e \\) clause.
-}
data ClauseFilter sig e = ClauseFilter
    { clauseFilterMode :: Mode
    -- ^ The mode \\( \\mu \\) (continue or stop).
    , clauseFilterEffect :: e
    -- ^ The effect type of the clause body expression.
    }

-- | Describes a full handler filter \\( H \\).
data Filter sig e = Filter
    { filterClauses :: Map sig (ClauseFilter sig e)
    -- ^ The map \( \bar{C} \) of operation signatures to their clause definitions.
    , filterEffect :: e
    -- ^ The effect type \\( E \\) of the final expression (implicit \\( \\mathsf{return} \\) clause).
    }

{- | Typeclass for effect algebras.

Defines the structure required for an effect type \\( E \\) over a signature \\( \\Sigma \\) (represented by @sig@).
-}
class (Lattice e, Monoid e) => Effect e sig where
    -- | Creates a singleton effect containing a basic operation.
    basic :: sig -> e

    {- | Extracts the set of operations present in an effect.

    Needed for typechecking of a \( \text{handle } e \text{ with } h \) expression.
    -}
    opsIn :: e -> Set sig

    {- | Applies a handler filter \\( H \\) to an effect \\( E \\).

    This corresponds to the function \\( \\mathcal{F}_H(E) \\) in the theory.
    -}
    applyFilter :: e -> Filter sig e -> e

{- | A concrete implementation of effects as sets of operations.

Here, an effect \\( E \\) is simply a subset of the signature \\( \\Sigma \\).
-}
newtype EffectSet s = EffectSet (Set s)
    deriving (Show, Eq)

-- | Subtyping is set inclusion: \\( E_1 \\subseteq E_2 \\).
instance (Ord s) => PartialOrd (EffectSet s) where
    (EffectSet e1) <: (EffectSet e2) = e1 `isSubsetOf` e2

-- | Lattice operations are set union and intersection.
instance (Ord s) => Lattice (EffectSet s) where
    join (EffectSet e1) (EffectSet e2) = EffectSet $ e1 `union` e2
    meet (EffectSet e1) (EffectSet e2) = EffectSet $ e1 `intersection` e2

instance (Ord s) => Semigroup (EffectSet s) where
    (EffectSet s1) <> (EffectSet s2) = EffectSet (s1 `union` s2)

instance (Ord s) => Monoid (EffectSet s) where
    mempty = EffectSet Set.empty
    mappend = (<>)

instance (Ord sig) => Effect (EffectSet sig) sig where
    basic op = EffectSet (singleton op)
    opsIn (EffectSet e) = e

    -- \| Applies the filter by replacing handled operations with their clause effects.
    applyFilter (EffectSet e) h =
        let
            cs = filterClauses h
            (EffectSet e') = filterEffect h
            flat op = case Map.lookup op cs of
                Nothing ->
                    Set.singleton op -- Not handled: keep the operation
                Just (ClauseFilter _ (EffectSet cEf)) ->
                    cEf -- Handled: replace with clause effect
         in
            EffectSet (e' `union` foldMap flat e)
