{- |
Module      : Nat
Description : Representation of natural numbers.

This module provides an abstract interface and a concrete implementation for
natural numbers (\\( \\mathbb{N} \\))
-}
module Nat (Nat (..), Peano (..), natSum, natDiff, natDiv) where

{- | A typeclass for types representing natural numbers \\( \\mathbb{N} \\).

Provides the basic constants and predicates required for the calculus examples.
-}
class (Enum a) => Nat a where
    -- | The zero element (\\( 0 \\)).
    zero :: a

    -- | Predicate testing if a number is zero.
    isZero :: a -> Bool

natSum :: (Nat a, Nat b) => a -> b -> b
natSum a b
    | isZero a = b
    | otherwise = succ $ natSum (pred a) b

natDiff :: (Nat a, Nat b) => a -> b -> a
natDiff a b
    | isZero b = a
    | otherwise = natDiff (pred a) (pred b)

natDiv :: (Nat a, Nat b) => a -> b -> a
natDiv a b
    | isZero b = undefined
    | fromEnum a < fromEnum b = zero
    | otherwise = succ $ natDiv (natDiff a b) b

-- | Inductively defined Peano representation of natural numbers.
data Peano
    = -- | The number \\( 0 \\).
      Zero
    | -- | The successor function \\( \\mathsf{succ}(n) \\).
      Succ Peano
    deriving (Show, Eq)

instance Enum Peano where
    succ n = Succ n
    pred Zero = undefined
    pred (Succ n) = n
    fromEnum Zero = 0
    fromEnum (Succ n) = 1 + fromEnum n
    toEnum 0 = Zero
    toEnum n
        | n < 0 = undefined
        | otherwise = Succ $ toEnum $ n - 1

instance Nat Peano where
    zero = Zero
    isZero Zero = True
    isZero _ = False
