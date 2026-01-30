{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Effects.Nondeterminism
Description : Algebraic effects for Non-Determinism.

This module implements the non-deterministic choice effect.
It provides:

*   __Signature__: A single @Choose@ operation.
*   __Semantics__: An interpretation into the List monad @[]@ (representing multiple possible outcomes).
-}
module Effects.Nondeterminism (
    NDSig (..),
)
where

import Language
import Types (ValType (BoolType))

{- | The signature for non-determinism.

Represents the singleton signature \( \Sigma = \{ \mathsf{choose} \} \).
-}
data NDSig = Choose deriving (Show, Eq, Ord)

{- | Typing for the choose operation.

\[
\mathsf{choose} : \mathbf{Unit} \to \mathsf{Bool}
\]

The operation returns a boolean value, but which one is determined non-deterministically.
-}
instance Sig NDSig where
    arity Choose = ([], BoolType)

{- | Monadic semantics for non-determinism.

Uses the standard List monad @[]@ (equivalent to the monad \( \mathcal{L}(X) \) or \( \mathcal{P}(X) \) from Example 2).

*   __Implementation__: \( \mathsf{choose} \) branches into a list containing both \( \text{true} \) and \( \text{false} \).
    \[
    \mathsf{run}_{\mathsf{choose}} = [\mathsf{true}, \mathsf{false}]
    \]
-}
instance MonSem [] NDSig where
    run Choose [] = [BoolVal True, BoolVal False]
    run Choose _ = undefined

-- instance ParsableSig NDSig where
--     bindOp "choose" = Just Choose
--     bindOp _ = Nothing
