{-# LANGUAGE FunctionalDependencies #-}

{- |
Module      : Core
Description : Abstract framework for monadic operational semantics.

This module implements an abstract framework for small-step monadic operational semantics.
It provides the definitions for configurations, results, and the lifting of
language-specific reductions to total monadic functions on configurations.
-}
module Core (
    Res (..),
    Conf (..),
    Valuable (..),
    Reducible (..),
    reduceStep,
    reduceMnStep,
    evalLoop,
    evalFin,
)
where

{- | Abstract interface for expressions that can be viewed as values.

This class captures the relationship between the set of expressions \( \mathbf{Exp} \)
and the set of values \( \mathbf{Val} \). It assumes an injection
\\( \\mathsf{ret} : \\mathbf{Val} \\to \\mathbf{Exp} \\) and provides the partial inverse.
-}
class Valuable e v | e -> v where
    {- | Attempts to project an expression into a value.
    Returns 'Just' \( v \) if the expression is a value (i.e., in the image of \( \mathsf{ret} \)),
    'Nothing' otherwise.
    -}
    toVal :: e -> Maybe v

{- | Abstract interface for the monadic one-step reduction.

This represents the reduction relation \( \to \subseteq \mathbf{Exp} \times M \mathbf{Exp} \),
where \( M \) is the monad modeling computational effects.
-}
class (Monad m, Valuable e v) => Reducible m e v where
    {- | The partial function \( (\to) \) representing a single computation step.

    * Returns 'Just' \( \alpha \) if the expression reduces to \( \alpha \in M \mathbf{Exp} \).
    * Returns 'Nothing' if the expression cannot be reduced (either it is a value or stuck).
    -}
    reduce :: e -> Maybe (m e)

{- | The set of results \( \mathbf{Res} = \mathbf{Val} + \mathbf{Wr} \).

A result represents the outcome of a finite computation: either successful
termination with a value or a stuck computation.
-}
data Res v
    = -- | Successful termination with a value \( v \).
      Ok v
    | -- | Represents a stuck computation.
      Wr
    deriving (Show)

{- | The set of configurations \( \mathbf{Conf} = \mathbf{Exp} + \mathbf{Res} \).

Configurations include both active expressions and terminal results.
This allows the partial reduction relation on expressions to be extended
to a total function on configurations.
-}
data Conf e v
    = -- | An active computation state (\\( \\mathbf{Exp} \\)).
      ExpConf e
    | -- | A terminal state (\\( \\mathbf{Res} \\)).
      ResConf (Res v)
    deriving (Show)

{- | The total monadic one-step reduction function on configurations.

This function corresponds to the relation \( \xrightarrow{\text{step}} \subseteq \mathbf{Conf} \times M \mathbf{Conf} \)
(shown as the function \( \mathsf{step} \) in Figure 1).

It implements the four specific rules from the paper:

1.  __RES__: A result reduces to itself (lifted to the monad).
2.  __RET__: A value expression reduces to the corresponding monadic result.
3.  __EXP__: An expression reduces according to the underlying 'reduce' function.
4.  __WRONG__: An expression that is not a value and cannot reduce becomes \( \mathbf{wrong} \).
-}
reduceStep :: (Reducible m e v) => Conf e v -> m (Conf e v)
reduceStep c = case c of
    ResConf _ -> pure c
    ExpConf e ->
        case toVal e of
            Just v -> pure (ResConf (Ok v))
            Nothing -> case reduce e of
                Just m_e' -> ExpConf <$> m_e'
                Nothing -> pure (ResConf Wr)

{- | Performs a reduction step on a monadic configuration.

This corresponds to the Kleisli extension of the step function, denoted as
\\( \\mathsf{step}^\\dagger \\) or the small-step reduction \( \Rightarrow \) on \( M \mathbf{Conf} \).

\[
c \Rightarrow c' \iff \mathsf{step}^\dagger(c) = c'
\]
-}
reduceMnStep :: (Reducible m e v) => m (Conf e v) -> m (Conf e v)
reduceMnStep c = c >>= reduceStep

{- | Repeatedly applies reduction steps until a result is reached.

This function computes the "monadic reflexive and transitive closure"
\\( \\xrightarrow{\\text{step}}^\\star \\).

If the computation terminates, it yields a result from \( \mathbf{Res} \).
If the monad supports non-termination or the expression diverges, this function may not return.
-}
evalLoop :: (Reducible m e v) => m (Conf e v) -> m (Res v)
evalLoop m_conf = do
    conf <- m_conf
    case conf of
        ResConf r -> pure r
        ExpConf _ -> evalLoop (reduceStep conf)

{- | Computes the finitary semantics of an expression.

This corresponds to the function \( [\![ e ]\!]_\star : \mathbf{Exp} \to M \mathbf{Res} + \{\infty\} \).
-}
evalFin :: (Reducible m e v) => e -> m (Res v)
evalFin e = evalLoop $ pure $ ExpConf e
