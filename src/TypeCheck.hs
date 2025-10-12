{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeCheck (
    ValType,
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

data Error e
    = UnboundVariable Identifier
    | ArityMismatch Int Int
    | ReturnTypeMismatch (ExpType e) (ExpType e)
    | ArgTypeMismatch (ValType e) (ValType e)
    | UnapplicableType (ValType e)
    | TypeMismatch (ValType e) (ValType e)
    deriving (Show)

type Context e = Map.Map Identifier (ValType e)

type Failable a e = Either (Error e) a

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
        unless (t''e' <: ExpType (t', e)) $ Left $ ReturnTypeMismatch (ExpType (t', e)) t''e'
        pure $ ArrType r

typeOfClause :: (Sig sig, Ord sig, Effect e sig) => sig -> Clause sig e -> Context e -> Failable (ExpType e) e
typeOfClause op clause ctx = do
    let (paramTypes, _) = arity op
    let params = clauseParams clause
    unless (length params == length paramTypes) $
        Left $
            ArityMismatch (length params) (length paramTypes)

    let clauseCtx = Map.union (Map.fromList (zip params paramTypes)) ctx
    typeOfExp (clauseBody clause) clauseCtx

require :: (Sig sig, Ord sig, Effect e sig) => Val sig e -> ValType e -> Context e -> Failable () e
require x t c = do
    t' <- typeOfVal x c
    if (t <: t') then Right () else Left $ TypeMismatch t t'

typeOfExp :: (Sig sig, Ord sig, Effect e sig) => Exp sig e -> Context e -> Failable (ExpType e) e
typeOfExp e ctx = case e of
    Ret v -> ExpType <$> (,mempty) <$> (typeOfVal v ctx)
    App f x -> do
        fT <- typeOfVal f ctx
        t2 <- typeOfVal x ctx
        case fT of
            (ArrType (ArrType' (t1, e', t))) -> if (t2 <: t1) then (pure $ ExpType (t, e')) else Left $ ArgTypeMismatch t1 t2
            _ -> Left $ UnapplicableType fT
    If p bT bE -> do
        require p BoolType ctx
        tT <- typeOfExp bT ctx
        tE <- typeOfExp bE ctx
        pure $ tT `join` tE
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
    Do x e1 e2 -> do
        ExpType (t1, ef1) <- typeOfExp e1 ctx
        let c' = Map.insert x t1 ctx
        ExpType (t2, ef2) <- typeOfExp e2 c'
        pure $ ExpType (t2, ef1 <> ef2)
    Magic op vs -> do
        let (vsTypes, t) = arity op
        when (length vs /= length vsTypes) $ Left $ ArityMismatch (length vs) (length vsTypes)
        forM_ (zip vs vsTypes) $ \(v, vType) -> do
            require v vType ctx
        pure $ ExpType (t, basic op)
    HandleWith e (Handler cs (x, e')) -> do
        ExpType (t, ef) <- typeOfExp e ctx
        ExpType (t', ef') <- typeOfExp e' (Map.insert x t ctx)

        stopTs <-
            Map.traverseWithKey
                (\op c -> typeOfClause op c ctx)
                ( Map.filter
                    (\c -> clauseMode c == Stop)
                    (Map.restrictKeys cs (opsIn ef))
                )
        let t'' = Map.foldr join t' (Map.map (\(ExpType (t, _)) -> t) stopTs)

        clauseFilters <-
            Map.traverseWithKey
                ( \op c -> do
                    ExpType (cT, cEf) <- typeOfClause op c ctx
                    case clauseMode c of
                        Continue -> do
                            let (_, t) = arity op
                            unless (cT <: t) $ Left $ TypeMismatch t cT
                        Stop -> unless (cT <: t'') $ Left $ TypeMismatch t'' cT
                    pure $ ClauseFilter (clauseMode c) cEf
                )
                cs

        let h = Filter clauseFilters ef'

        pure $ ExpType (t'', applyFilter ef h)

typeOf :: (Sig sig, Ord sig, Effect e sig) => (Exp sig e) -> Failable (ExpType e) e
typeOf e = typeOfExp e Map.empty
