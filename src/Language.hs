{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Language (
    Val (..),
    Exp (..),
    MonSem (..),
    Sig (..),
    Handler (..),
    Clause (..),
    Mode (..),
    subst,
    reduce,
)
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate)
import Nat (Nat(..))

type Identifier = String

data Val sig where
    NumVal :: (Nat n, Show n) => n -> Val sig
    BoolVal :: Bool -> Val sig
    LamVal :: Identifier -> Exp sig -> Val sig
    RecLamVal :: Identifier -> Identifier -> Exp sig -> Val sig
    IdentifierVal :: Identifier -> Val sig

instance Show sig => Show (Val sig) where
    show v = case v of
        NumVal n -> show n
        BoolVal b -> show b
        LamVal x e -> "λ" ++ x ++ "." ++ show e
        RecLamVal f x e -> "rec " ++ f ++ ".λ" ++ x ++ "." ++ show e
        IdentifierVal x -> x

data Mode = Continue | Stop

data Clause sig = Clause
    { clauseMode :: Mode
    , clauseParams :: [Identifier]
    , clauseBody :: Exp sig
    }

data Handler sig = Handler
    { handlerClauses :: Map sig (Clause sig)
    , handlerFinal :: (Identifier, Exp sig)
    }

instance (Show sig) => Show (Handler sig) where
    show (Handler cs (x, e)) =
        "(("
        ++ intercalate ", " [showClause op c | (op, c) <- Map.toList cs]
        ++ "), "
        ++ x ++ "->" ++ show e
        ++ ")"
      where
        showClause op (Clause m p b) =
            show op ++ show p ++ "(->" ++ arrowMode m ++ ")" ++ show b

        arrowMode Continue = "c"
        arrowMode Stop     = "s"

substHandler :: Identifier -> Val sig -> Handler sig -> Handler sig
substHandler varName val (Handler clauses (finalVar, finalBody)) =
    let newClauses = substClause varName val <$> clauses
        newFinalBody =
            if finalVar == varName
                then finalBody
                else subst varName val finalBody
     in Handler newClauses (finalVar, newFinalBody)

substClause :: Identifier -> Val sig -> Clause sig -> Clause sig
substClause varName val (Clause mode params body) =
    let newBody =
            if varName `elem` params
                then body
                else subst varName val body
     in Clause mode params newBody

data Exp sig
    = App (Val sig) (Val sig)
    | Ret (Val sig)
    | Do Identifier (Exp sig) (Exp sig)
    | Magic sig [Val sig]
    | HandleWith (Exp sig) (Handler sig)
    | If (Val sig) (Exp sig) (Exp sig)
    | Plus (Val sig) (Val sig)
    | Minus (Val sig) (Val sig)
    | And (Val sig) (Val sig)
    | Or (Val sig) (Val sig)
    | IsZero (Val sig)
    | Suc (Val sig)
    | Pred (Val sig)

instance (Show sig) => Show (Exp sig) where
    show e =
        "("
            ++ ( case e of
                    App v1 v2 -> show v1 ++ " " ++ show v2
                    Ret v -> "return " ++ show v
                    Do x e1 e2 -> "do " ++ show x ++ " = " ++ show e1 ++ "; " ++ show e2
                    Magic s vs -> show s ++ "(" ++ show vs ++ ")"
                    HandleWith e1 h -> "handle " ++ show e1 ++ " with " ++ show h
                    If p bThen bElse -> "if " ++ "(" ++ show p ++ ") then " ++ show bThen ++ " else " ++ show bElse
                    Plus (a) (b) -> show a ++ " + " ++ show b
                    Minus (a) (b) -> show a ++ " - " ++ show b
                    And (a) (b) -> show a ++ " && " ++ show b
                    Or (a) (b) -> show a ++ " || " ++ show b
                    IsZero (n) -> "iszero " ++ show n
                    Suc (n) -> "succ " ++ show n
                    Pred (n) -> "pred " ++ show n
               )
            ++ ")"

substVal :: Identifier -> Val sig -> Val sig -> Val sig
substVal varName val (IdentifierVal x) | x == varName = val
substVal varName val (LamVal x body)
    | x == varName = LamVal x body
    | otherwise = LamVal x (subst varName val body)
substVal varName val v@(RecLamVal f x body)
    | f == varName || x == varName = v
    | otherwise = RecLamVal f x (subst varName val body)
substVal _ _ v = v

subst :: Identifier -> Val sig -> Exp sig -> Exp sig
subst varName val e = case e of
    App v1 v2 -> App (substVal varName val v1) (substVal varName val v2)
    Ret v -> Ret (substVal varName val v)
    Do x e1 e2 ->
        if x == varName
            then Do x (subst varName val e1) e2
            else Do x (subst varName val e1) (subst varName val e2)
    Magic s vs -> Magic s (substVal varName val <$> vs)
    HandleWith e1 h -> HandleWith (subst varName val e1) (substHandler varName val h)
    If p bThen bElse -> If (substVal varName val p) (subst varName val bThen) (subst varName val bElse)
    Plus (a) (b) -> Plus (substVal varName val a) (substVal varName val b)
    Minus (a) (b) -> Minus (substVal varName val a) (substVal varName val b)
    And (a) (b) -> And (substVal varName val a) (substVal varName val b)
    Or (a) (b) -> Or (substVal varName val a) (substVal varName val b)
    IsZero (n) -> IsZero (substVal varName val n)
    Suc (n) -> Suc (substVal varName val n)
    Pred (n) -> Pred (substVal varName val n)

substs :: [(Identifier, Val sig)] -> Exp sig -> Exp sig
substs substitutions expr = foldr (\(var, val) e -> subst var val e) expr substitutions

class Sig s where
    arity :: s -> Int

class (Monad m, Sig sig) => MonSem m sig where
    run :: sig -> [Val sig] -> m (Val sig)

reducePure :: (Ord sig) => Exp sig -> Maybe (Exp sig)
reducePure e = case e of
    App (LamVal var body) arg -> Just $ subst var arg body
    App v@(RecLamVal f x body) arg -> Just $ (subst f v . subst x arg) body
    If (BoolVal True) bThen _ -> Just bThen
    If (BoolVal False) _ bElse -> Just bElse
    Plus (NumVal a) (NumVal b) -> Just $ Ret $ NumVal $ f a b
        where
            f x y
                | isZero x = y
                | otherwise = succ $ f x (pred y)
    Minus (NumVal a) (NumVal b) -> Just $ Ret $ NumVal $ f a b
        where
            f x y
                | isZero y = x
                | otherwise = f (pred x) (pred y)
    And (BoolVal a) (BoolVal b) -> Just $ Ret $ BoolVal (a && b)
    Or (BoolVal a) (BoolVal b) -> Just $ Ret $ BoolVal (a || b)
    IsZero (NumVal n) -> Just $ Ret $ BoolVal $ isZero n
    Suc (NumVal n) -> Just $ Ret $ NumVal $ succ n
    Pred (NumVal n) -> Just $ Ret $ NumVal $ pred n
    HandleWith (Do y e1 e2) h -> Just $ HandleWith e1 (h{handlerFinal = (y, HandleWith e2 h)})
    HandleWith (Ret v) (Handler _ (x, e')) -> Just $ Do x (Ret v) e'
    HandleWith (Magic op vs) (Handler cs (finalVar, finalBody)) ->
        case Map.lookup op cs of
            Just (Clause Continue params body) -> Just $ Do finalVar (substs (zip params vs) body) finalBody
            Just (Clause Stop params body) -> Just $ substs (zip params vs) body
            Nothing -> Just $ Do finalVar (Magic op vs) finalBody
    HandleWith e1 h -> HandleWith <$> reducePure e1 <*> pure h
    _ -> Nothing

reduce :: (MonSem m sig, Ord sig) => Exp sig -> Maybe (m (Exp sig))
reduce e =
    case reducePure e of
        Just e' -> Just (pure e')
        Nothing ->
            case e of
                Magic op vs -> Just $ Ret <$> run op vs -- EFFECT
                Do var (Ret val) e2 -> Just $ pure $ subst var val e2 -- RET
                Do var e1 e2 ->
                    -- DO
                    case reduce e1 of
                        Just m_e1' -> Just $ (\e1' -> Do var e1' e2) <$> m_e1'
                        Nothing -> Nothing
                _ -> Nothing
