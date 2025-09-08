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
    signature,
 )

import Control.Monad (unless, forM_, when)
import Control.Monad.State
import Data.Map qualified as Map

data Error
    = UnboundVariable Identifier
    | ArityMismatch Int Int
    | ReturnTypeMismatch ExpType ExpType
    | ArgTypeMismatch ValType ValType
    | UnapplicableType ValType
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
    LamVal x b r@(ArrType' (t, e, t')) -> do
        let c' = Map.insert x t c
        t''e' <- typeOfExp b c'
        unless (t''e' <: ExpType (t', e)) $ Left $ ReturnTypeMismatch (ExpType (t', e)) t''e'
        pure $ ArrType r
    RecLamVal f x b r@(ArrType' (t, e, t')) -> do
        let c' = Map.insert f (ArrType r) (Map.insert x t c)
        t''e' <- typeOfExp b c'
        unless (t''e' <: ExpType (t', e)) $ Left $ ReturnTypeMismatch (ExpType (t', e)) t''e'
        pure $ ArrType r

typeOfExp :: (Sig sig) => Exp sig -> Context -> Failable ExpType
typeOfExp e c = case e of
    Ret v -> ExpType <$> (,()) <$> (typeOfVal v c)
    App f x -> do
        fT <- typeOfVal f c
        t2 <- typeOfVal x c
        case fT of
            (ArrType (ArrType' (t1, e, t))) -> if (t2 <: t1) then (pure $ ExpType (t, e)) else Left $ ArgTypeMismatch t1 t2
            _ -> Left $ UnapplicableType fT
    -- If p bT bE -> do
    --     pType <- typeOfVal p c
    --     unify pType BoolType

    --     (bTType, ()) <- typeOfExp bT c
    --     (bEType, ()) <- typeOfExp bE c
    --     unify bTType bEType
-- 
    --      pure (bTType, ())
--     Plus a b -> do
--         aType <- typeOfVal a c
--         unify aType NatType
-- 
--         bType <- typeOfVal b c
--         unify bType NatType
-- 
--         pure (NatType, ())
--     Minus a b -> do
--         aType <- typeOfVal a c
--         unify aType NatType
-- 
--         bType <- typeOfVal b c
--         unify bType NatType
-- 
--         pure (NatType, ())
--     And a b -> do
--         aType <- typeOfVal a c
--         unify aType BoolType
-- 
--         bType <- typeOfVal b c
--         unify bType BoolType
-- 
--         pure (BoolType, ())
--     Or a b -> do
--         aType <- typeOfVal a c
--         unify aType BoolType
-- 
--         bType <- typeOfVal b c
--         unify bType BoolType
-- 
--         pure (BoolType, ())
--     IsZero a -> do
--         aType <- typeOfVal a c
--         unify aType NatType
--         pure (BoolType, ())
--     Suc a -> do
--         aType <- typeOfVal a c
--         unify aType NatType
--         pure (NatType, ())
--     Pred a -> do
--         aType <- typeOfVal a c
--         unify aType NatType
--         pure (NatType, ())
--     Do x e1 e2 -> do
--         (e1Type, ()) <- typeOfExp e1 c
--         let c' = Map.insert x e1Type c
--         typeOfExp e2 c'
--     Magic op vs -> do
--         let (vsTypes, t) = signature op
--         when (length vs /= length vsTypes) $ lift $ Left $ ArityMismatch (length vs) (length vsTypes)
--         forM_ (zip vs vsTypes) $ \(v, vType) -> do
--             vType' <- typeOfVal v c
--             unify vType' vType
--         pure (t, ())
--     HandleWith e1 (Handler _cs (x, e')) -> do
--         (t, ()) <- typeOfExp e1 c
--         let c' = Map.insert x t c
--         (t', ()) <- typeOfExp e' c'
--         pure (t', ())
-- 
-- typeOf :: (Sig sig) => Exp sig -> Failable ExpType
typeOf e = do
    -- ((t, ()), (_, s)) <- runStateT (typeOfExp e Map.empty) (0, Map.empty)
    -- let t' = apply s t
    -- pure (t', ())
    undefined
