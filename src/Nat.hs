module Nat (Nat(..), Peano) where

class (Enum a) => Nat a where
    zero :: a
    isZero :: a -> Bool

data Peano = Zero | Succ Peano deriving (Show, Eq)

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
