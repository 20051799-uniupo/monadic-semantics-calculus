module TypeCheck (
    ValType,
    typeOf,
) where

import Types (ValType(..), ExpType (..), ArrType' (..), SubType(..))

import Language (
    Exp (..),
    Identifier,
    Val (..),
    Sig,
    Handler(..),
    arity,
 )

import Control.Monad (unless, forM_, when)
import Data.Map qualified as Map

data Error
    = UnboundVariable Identifier
    | ArityMismatch Int Int
    | ReturnTypeMismatch ExpType ExpType
    | ArgTypeMismatch ValType ValType
    | UnapplicableType ValType
    | TypeMismatch ValType ValType
    deriving (Show)

type Context = Map.Map Identifier ValType

type Failable a = Either Error a

typeOfVal :: (Sig sig) => Val sig -> Context -> Failable ValType
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
    RecLamVal f r@(ArrType' (t, e, t')) x b  -> do
        let c' = Map.insert f (ArrType r) (Map.insert x t c)
        t''e' <- typeOfExp b c'
        unless (t''e' <: ExpType (t', e)) $ Left $ ReturnTypeMismatch (ExpType (t', e)) t''e'
        pure $ ArrType r

require :: (Sig sig) => Val sig -> ValType -> Context -> Failable ()
require x t c = do
    t' <- typeOfVal x c
    if (t <: t') then Right() else Left $ TypeMismatch t t'

typeOfExp :: (Sig sig) => Exp sig -> Context -> Failable ExpType
typeOfExp e c = case e of
    Ret v -> ExpType <$> (,()) <$> (typeOfVal v c)
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
        pure $ ExpType (NatType, ())
    Minus a b -> do
        require a NatType c
        require b NatType c
        pure $ ExpType (NatType, ())
    And a b -> do
        require a BoolType c
        require b BoolType c
        pure $ ExpType (BoolType, ())
    Or a b -> do
        require a BoolType c
        require b BoolType c
        pure $ ExpType (BoolType, ())
    IsZero a -> do
        require a NatType c
        pure $ ExpType (BoolType, ())
    Suc a -> do
        require a NatType c
        pure $ ExpType (NatType, ())
    Pred a -> do
        require a NatType c
        pure $ ExpType (NatType, ())
    Do x e1 e2 -> do
        ExpType (t1, ()) <- typeOfExp e1 c
        let c' = Map.insert x t1 c
        typeOfExp e2 c'
    Magic op vs -> do
        let (vsTypes, t) = arity op
        when (length vs /= length vsTypes) $ Left $ ArityMismatch (length vs) (length vsTypes)
        forM_ (zip vs vsTypes) $ \(v, vType) -> do
            require v vType c
        pure $ ExpType (t, ())
    HandleWith e1 (Handler _ (x, e')) -> do
        ExpType (t, ()) <- typeOfExp e1 c
        let c' = Map.insert x t c
        ExpType (t', ()) <- typeOfExp e' c'
        pure $ ExpType (t', ())

typeOf :: (Sig sig) => Exp sig -> Failable ExpType
typeOf e = typeOfExp e Map.empty
