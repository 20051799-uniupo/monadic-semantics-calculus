{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Effects.Exceptions
-- Description : Algebraic effects for Exceptions.
--
-- This module implements the exception effect.
--
-- It provides:
--
-- *   __Signature__: A generic @Raise@ operation parameterized by the exception type.
-- *   __Semantics__: An interpretation into the standard @Either e@ monad.
module Effects.Exceptions
  ( ExceptionSig (..),
  )
where

import Language
import Types(ValType(BotType))

-- | The signature defined for exceptions.
--
-- Represents the family of operations \( \Sigma = \{ \mathsf{raise}\langle e \rangle \mid e \in Exc \} \).
data ExceptionSig e = Raise e deriving (Show, Eq, Ord)

-- | Typing for the exception operation.
--
-- \[
-- \mathsf{raise}\langle e \rangle : \mathbf{Unit} \to \bot
-- \]
--
-- The return type is \( \bot \), indicating that the operation
-- aborts the current continuation and never returns a value.
instance Sig (ExceptionSig e) where
    arity (Raise _) = ([], BotType)

-- | Monadic semantics for exceptions.
--
-- Uses the @Either e@ monad (equivalent to the exception monad \( M(X) = Exc + X \)).
--
-- *   __Implementation__: \( \mathsf{raise}\langle e \rangle \) becomes \( \mathsf{Left}\ e \).
instance MonSem (Either e) (ExceptionSig e) where
  run (Raise e) [] = Left e
  run _ _ = undefined
