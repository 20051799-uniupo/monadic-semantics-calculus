module TypeCheck (
    ValType,
    typeOf,
) where

import Types (ValType(..), ExpType)

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

type Substitutions = Map.Map Int ValType
type InferenceState = (Int, Substitutions)

data Error
    = UnboundVariable Identifier
    | TypesNotUnifiable ValType ValType
    | SelfReferentialBind Int
    | ArityMismatch Int Int
    deriving (Show)

type InferM a = StateT InferenceState (Either Error) a

fresh :: InferM ValType
fresh = state $ \(count, substs) -> (TypeVar count, (count + 1, substs))

apply :: Substitutions -> ValType -> ValType
apply s t = case t of
    TypeVar i -> Map.findWithDefault t i s
    ArrType t1 e t2 -> ArrType (apply s t1) e (apply s t2)
    _ -> t

-- unify :: ValType -> ValType -> (ValType -> ValType -> Error) -> InferM ()
unify :: ValType -> ValType -> InferM ()
unify t1 t2 = do
    (_, s) <- get
    let t1' = apply s t1
    let t2' = apply s t2
    unless
        (t1' == t2')
        ( case (t1', t2') of
            (TypeVar i, t) -> varBind i t
            (t, TypeVar i) -> varBind i t
            (ArrType x1 _ e1, ArrType x2 _ e2) -> unify x1 x2 >> unify e1 e2
            _ -> lift $ Left $ TypesNotUnifiable t1' t2'
        )

varBind :: Int -> ValType -> InferM ()
varBind i t =
    if (createsCycle i t)
        then lift $ Left $ SelfReferentialBind i
        else modify $ \(c, s) -> (c, Map.insert i t s)

createsCycle :: Int -> ValType -> Bool
createsCycle i t = case t of
    TypeVar j -> i == j
    (ArrType t1 _ t2) -> createsCycle i t1 || createsCycle i t2
    _ -> False

type Context = Map.Map Identifier ValType

typeOfVal :: (Sig sig) => Val sig -> Context -> InferM ValType
typeOfVal v c = case v of
    NatVal _ -> pure NatType
    BoolVal _ -> pure BoolType
    IdentifierVal x -> case Map.lookup x c of
        Just t -> pure t
        Nothing -> lift $ Left $ UnboundVariable x
    LamVal x e -> do
        xType <- fresh
        let c' = Map.insert x xType c
        (eType, ()) <- typeOfExp e c'
        pure $ ArrType xType () eType
    RecLamVal f x e -> do
        fType <- fresh
        xType <- fresh
        let c' = Map.insert f fType (Map.insert x xType c)
        (t_e, ()) <- typeOfExp e c'
        unify fType (ArrType xType () t_e)
        pure fType

typeOfExp :: (Sig sig) => Exp sig -> Context -> InferM ExpType
typeOfExp e c = case e of
    Ret v -> (,()) <$> (typeOfVal v c)
    App f x -> do
        fType <- typeOfVal f c
        xType <- typeOfVal x c
        resType <- fresh
        unify fType (ArrType xType () resType)
        pure (resType, ())
    If p bT bE -> do
        pType <- typeOfVal p c
        unify pType BoolType

        (bTType, ()) <- typeOfExp bT c
        (bEType, ()) <- typeOfExp bE c
        unify bTType bEType

        pure (bTType, ())
    Plus a b -> do
        aType <- typeOfVal a c
        unify aType NatType

        bType <- typeOfVal b c
        unify bType NatType

        pure (NatType, ())
    Minus a b -> do
        aType <- typeOfVal a c
        unify aType NatType

        bType <- typeOfVal b c
        unify bType NatType

        pure (NatType, ())
    And a b -> do
        aType <- typeOfVal a c
        unify aType BoolType

        bType <- typeOfVal b c
        unify bType BoolType

        pure (BoolType, ())
    Or a b -> do
        aType <- typeOfVal a c
        unify aType BoolType

        bType <- typeOfVal b c
        unify bType BoolType

        pure (BoolType, ())
    IsZero a -> do
        aType <- typeOfVal a c
        unify aType NatType
        pure (BoolType, ())
    Suc a -> do
        aType <- typeOfVal a c
        unify aType NatType
        pure (NatType, ())
    Pred a -> do
        aType <- typeOfVal a c
        unify aType NatType
        pure (NatType, ())
    Do x e1 e2 -> do
        (e1Type, ()) <- typeOfExp e1 c
        let c' = Map.insert x e1Type c
        typeOfExp e2 c'
    Magic op vs -> do
        let (vsTypes, t) = signature op
        when (length vs /= length vsTypes) $ lift $ Left $ ArityMismatch (length vs) (length vsTypes)
        forM_ (zip vs vsTypes) $ \(v, vType) -> do
            vType' <- typeOfVal v c
            unify vType' vType
        pure (t, ())
    HandleWith e1 (Handler _cs (x, e')) -> do
        (t, ()) <- typeOfExp e1 c
        let c' = Map.insert x t c
        (t', ()) <- typeOfExp e' c'
        pure (t', ())

typeOf :: (Sig sig) => Exp sig -> Either Error ExpType
typeOf e = do
    ((t, ()), (_, s)) <- runStateT (typeOfExp e Map.empty) (0, Map.empty)
    let t' = apply s t
    pure (t', ())
