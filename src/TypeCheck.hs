{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeCheck (
    ValType,
    typeOf,
) where

import Types (ArrType' (..), Effect (..), ExpType (..), Lattice (..), PartialOrd (..), ValType (..))

import Language (
    Exp (..),
    Handler (..),
    Identifier,
    Sig,
    Val (..),
    arity,
 )

import Control.Monad (forM_, unless, when)
import Data.Map qualified as Map
import Prelude hiding (filter)

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

typeOfVal :: (Sig sig, Effect e sig) => Val sig e -> Context e -> Failable (ValType e) e
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

require :: (Sig sig, Effect e sig) => Val sig e -> ValType e -> Context e -> Failable () e
require x t c = do
    t' <- typeOfVal x c
    if (t <: t') then Right () else Left $ TypeMismatch t t'

typeOfExp :: (Sig sig, Effect e sig) => Exp sig e -> Context e -> Failable (ExpType e) e
typeOfExp e c = case e of
    Ret v -> ExpType <$> (,mempty) <$> (typeOfVal v c)
    App f x -> do
        fT <- typeOfVal f c
        t2 <- typeOfVal x c
        case fT of
            (ArrType (ArrType' (t1, e', t))) -> if (t2 <: t1) then (pure $ ExpType (t, e')) else Left $ ArgTypeMismatch t1 t2
            _ -> Left $ UnapplicableType fT
    If p bT bE -> do
        require p BoolType c
        tT <- typeOfExp bT c
        tE <- typeOfExp bE c
        pure $ tT `join` tE
    Plus a b -> do
        require a NatType c
        require b NatType c
        pure $ ExpType (NatType, mempty)
    Minus a b -> do
        require a NatType c
        require b NatType c
        pure $ ExpType (NatType, mempty)
    And a b -> do
        require a BoolType c
        require b BoolType c
        pure $ ExpType (BoolType, mempty)
    Or a b -> do
        require a BoolType c
        require b BoolType c
        pure $ ExpType (BoolType, mempty)
    IsZero a -> do
        require a NatType c
        pure $ ExpType (BoolType, mempty)
    Suc a -> do
        require a NatType c
        pure $ ExpType (NatType, mempty)
    Pred a -> do
        require a NatType c
        pure $ ExpType (NatType, mempty)
    Do x e1 e2 -> do
        ExpType (t1, ef1) <- typeOfExp e1 c
        let c' = Map.insert x t1 c
        ExpType (t2, ef2) <- typeOfExp e2 c'
        pure $ ExpType (t2, ef1 <> ef2)
    Magic op vs -> do
        let (vsTypes, t) = arity op
        when (length vs /= length vsTypes) $ Left $ ArityMismatch (length vs) (length vsTypes)
        forM_ (zip vs vsTypes) $ \(v, vType) -> do
            require v vType c
        pure $ ExpType (t, basic op)
    HandleWith e (Handler _ (x, e')) -> do
        ExpType (t, ef) <- typeOfExp e c
        let clausesEffects = Map.empty
        let c' = Map.insert x t c
        ExpType (t', ef') <- typeOfExp e' c'
        pure $ ExpType (t', (filter ef clausesEffects) <> ef')

typeOf :: (Sig sig, Effect e sig) => (Exp sig e) -> Failable (ExpType e) e
typeOf e = typeOfExp e Map.empty
