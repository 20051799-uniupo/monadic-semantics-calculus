{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : TypeCheck
Description : Type-and-effect checking algorithm.

This module implements the static analysis for the \( \Lambda_\Sigma \) calculus.
It checks whether expressions are well-typed and computes the static approximation
of their computational effects.
-}
module TypeCheck (
    typeOf,
) where

import Types (ArrType' (..), ClauseFilter (..), Effect (..), ExpType (..), Filter (..), Lattice (..), Mode (..), PartialOrd (..), ValType (..))

import Language (
    Clause (..),
    Exp (..),
    Handler (..),
    Identifier,
    Sig,
    Val (..),
    arity,
 )

import Control.Monad (forM_, unless, when)
import Data.Map qualified as Map

-- | Represents errors encountered during type checking.
data Error e
    = -- | Variable not found in the context \( \Gamma \).
      UnboundVariable Identifier
    | -- | Operation or function called with wrong number of arguments.
      ArityMismatch Int Int
    | -- | The body of a function does not match its declared return type/effect.
      ReturnTypeMismatch (ExpType e) (ExpType e)
    | -- | Argument type is not a subtype of the expected type.
      ArgTypeMismatch (ValType e) (ValType e)
    | -- | Attempting to apply a non-function value.
      UnapplicableType (ValType e)
    | -- | General type mismatch (expected, actual).
      TypeMismatch (ValType e) (ValType e)
    deriving (Show)

{- | The typing context \( \Gamma \).

Maps variables to their value types: \( \Gamma : \text{Identifier} \to \mathbf{ValType} \).
-}
type Context e = Map.Map Identifier (ValType e)

-- | Result type for the type checker (Either Error or Success).
type Failable a e = Either (Error e) a

{- | Infers the type of a value.

Corresponds to the judgment \( \Gamma \vdash v : T \).

Rules implemented:

*   __T-VAR__: Look up in \( \Gamma \).
*   __T-ABS__: Check lambda body extending \( \Gamma \).
-}
typeOfVal :: (Sig sig, Ord sig, Effect e sig) => Val sig e -> Context e -> Failable (ValType e) e
typeOfVal v c = case v of
    NatVal _ -> pure NatType
    BoolVal _ -> pure BoolType
    IdentifierVal x -> case Map.lookup x c of
        Just t -> pure t
        Nothing -> Left $ UnboundVariable x
    LamVal x t b -> do
        let c' = Map.insert x t c
        ExpType (t', e) <- typeOfExp b c'
        pure $ ArrType (ArrType' (t, e, t'))
    RecLamVal f r@(ArrType' (t, e, t')) x b -> do
        let c' = Map.insert f (ArrType r) (Map.insert x t c)
        t''e' <- typeOfExp b c'
        -- Check that the body respects the declared type signature
        unless (t''e' <: ExpType (t', e)) $ Left $ ReturnTypeMismatch (ExpType (t', e)) t''e'
        pure $ ArrType r

{- | Infers the type and effect of a single handler clause.

Used within the __T-HANDLER__ rule logic.

Given a clause \( op(\overline{x}) \to_\mu e \):

1. Checks arity of \( op \).
2. Types the body \( e \) in context extended with parameters \( \overline{x} \).
-}
typeOfClause :: (Sig sig, Ord sig, Effect e sig) => sig -> Clause sig e -> Context e -> Failable (ExpType e) e
typeOfClause op clause ctx = do
    let (paramTypes, _) = arity op
    let params = clauseParams clause
    unless (length params == length paramTypes) $
        Left $
            ArityMismatch (length params) (length paramTypes)

    let clauseCtx = Map.union (Map.fromList (zip params paramTypes)) ctx
    typeOfExp (clauseBody clause) clauseCtx

{- | Checks that a value is a subtype of the expected type.

Enforces \( \Gamma \vdash v : T' \) where \( T' \le T \).
-}
require :: (Sig sig, Ord sig, Effect e sig) => Val sig e -> ValType e -> Context e -> Failable () e
require x t c = do
    t' <- typeOfVal x c
    if (t <: t') then Right () else Left $ TypeMismatch t t'

{- | Infers the type and effect of an expression.

Corresponds to the judgment \( \Gamma \vdash e : T ! E \).
-}
typeOfExp :: (Sig sig, Ord sig, Effect e sig) => Exp sig e -> Context e -> Failable (ExpType e) e
typeOfExp e ctx = case e of
    -- Rule (T-RET): return v : T ! {epsilon}
    Ret v -> ExpType <$> (,mempty) <$> (typeOfVal v ctx)
    -- Rule (T-APP): v1 v2
    App f x -> do
        fT <- typeOfVal f ctx
        t2 <- typeOfVal x ctx
        case fT of
            (ArrType (ArrType' (t1, e', t))) ->
                -- Check contravariance on argument: t2 <= t1
                if (t2 <: t1)
                    then (pure $ ExpType (t, e'))
                    else Left $ ArgTypeMismatch t1 t2
            _ -> Left $ UnapplicableType fT
    If p bT bE -> do
        require p BoolType ctx
        tT <- typeOfExp bT ctx
        tE <- typeOfExp bE ctx
        -- Result is the join (LUB) of branches
        pure $ tT `join` tE

    -- Primitives
    Plus a b -> do
        require a NatType ctx
        require b NatType ctx
        pure $ ExpType (NatType, mempty)
    Minus a b -> do
        require a NatType ctx
        require b NatType ctx
        pure $ ExpType (NatType, mempty)
    And a b -> do
        require a BoolType ctx
        require b BoolType ctx
        pure $ ExpType (BoolType, mempty)
    Or a b -> do
        require a BoolType ctx
        require b BoolType ctx
        pure $ ExpType (BoolType, mempty)
    IsZero a -> do
        require a NatType ctx
        pure $ ExpType (BoolType, mempty)
    Suc a -> do
        require a NatType ctx
        pure $ ExpType (NatType, mempty)
    Pred a -> do
        require a NatType ctx
        pure $ ExpType (NatType, mempty)

    -- Rule (T-DO): do x = e1; e2
    -- Effects are sequenced: E1 <> E2
    Do x e1 e2 -> do
        ExpType (t1, ef1) <- typeOfExp e1 ctx
        let c' = Map.insert x t1 ctx
        ExpType (t2, ef2) <- typeOfExp e2 c'
        pure $ ExpType (t2, ef1 <> ef2)

    -- Rule (T-OP): op(vs)
    -- Effect is singleton {op}
    Magic op vs -> do
        let (vsTypes, t) = arity op
        when (length vs /= length vsTypes) $ Left $ ArityMismatch (length vs) (length vsTypes)
        forM_ (zip vs vsTypes) $ \(v, vType) -> do
            require v vType ctx
        pure $ ExpType (t, basic op)

    -- Rule (T-WITH): handle e with h
    -- This implements the logic in Figure 10.
    HandleWith e (Handler cs (x, e')) -> do
        -- 1. Type the handled expression: e : T ! E
        ExpType (t, ef) <- typeOfExp e ctx

        -- 2. Type the final clause (return): x:T |- e' : T' ! E'
        ExpType (t', ef') <- typeOfExp e' (Map.insert x t ctx)

        -- 3. Calculate the overall return type T''
        -- T'' is the join of the final clause type T' and the types of all Stop clauses.
        stopTs <-
            Map.traverseWithKey
                (\op c -> typeOfClause op c ctx)
                ( Map.filter
                    (\c -> clauseMode c == Stop)
                    (Map.restrictKeys cs (opsIn ef))
                )
        let t'' = Map.foldr join t' (Map.map (\(ExpType (t, _)) -> t) stopTs)

        -- 4. Type check all clauses and build the Filter H.
        -- (Rules T-CONTINUE and T-STOP)
        clauseFilters <-
            Map.traverseWithKey
                ( \op c -> do
                    ExpType (cT, cEf) <- typeOfClause op c ctx
                    case clauseMode c of
                        -- If mode is Continue, body must match operation return type
                        Continue -> do
                            let (_, t) = arity op
                            unless (cT <: t) $ Left $ TypeMismatch t cT
                        -- If mode is Stop, body must match overall handler return type T''
                        Stop -> unless (cT <: t'') $ Left $ TypeMismatch t'' cT
                    pure $ ClauseFilter (clauseMode c) cEf
                )
                cs

        let h = Filter clauseFilters ef'

        -- 5. Result is T'' ! Filter(E)
        pure $ ExpType (t'', applyFilter ef h)

{- | Main entry point for type checking a closed expression.

Returns the type and effect \( T ! E \) if well-typed, or an error.
-}
typeOf :: (Sig sig, Ord sig, Effect e sig) => (Exp sig e) -> Failable (ExpType e) e
typeOf e = typeOfExp e Map.empty
