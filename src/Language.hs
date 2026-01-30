{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Module      : Language
Description : Syntax and operational semantics of \( \Lambda_\Sigma \).

Syntax and operational semantics of \( \Lambda_\Sigma \).
-}
module Language (
    Identifier,
    Val (..),
    Exp (..),
    MonSem (..),
    Sig (..),
    -- ParsableSig(..),
    Handler (..),
    Clause (..),
)
where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map

import Nat (Nat (..))
import Types (ArrType' (..), Mode (..), ValType (ArrType))

import Core (Reducible (..), Valuable (..))

-- | Identifiers are represented as strings.
type Identifier = String

-- | The set of values \( \mathbf{Val} \).
data Val sig e where
    -- | Natural number values (generic over representation \( \mathbb{N} \)).
    NatVal :: (Nat n, Show n) => n -> Val sig e
    -- | Boolean values.
    BoolVal :: Bool -> Val sig e
    -- | Lambda abstraction \( \lambda x. e \).
    LamVal :: Identifier -> ValType e -> Exp sig e -> Val sig e
    -- | Recursive function \( \text{rec } f. \lambda x. e \).
    RecLamVal :: Identifier -> ArrType' e -> Identifier -> Exp sig e -> Val sig e
    -- | Variables (only used during substitution, technically not closed values).
    IdentifierVal :: Identifier -> Val sig e

instance (Show sig, Show e) => Show (Val sig e) where
    show v = case v of
        NatVal n -> show n
        BoolVal b -> show b
        LamVal x t b -> "λ" ++ x ++ ": " ++ show t ++ "." ++ show b
        RecLamVal f t x b -> "rec " ++ f ++ ": " ++ show (ArrType t) ++ "." ++ x ++ "." ++ show b
        IdentifierVal x -> x

{- | A handler clause for a specific operation.

Corresponds to \( op(\bar{x}) \to_\mu e \).
-}
data Clause sig e = Clause
    { clauseMode :: Mode
    -- ^ The mode \( \mu \).
    , clauseParams :: [Identifier]
    -- ^ The parameters bound to the operation arguments.
    , clauseBody :: Exp sig e
    -- ^ The body of the clause expression.
    }

{- | A handler definition \( h \):

\[
h ::= \overline{c}, x \mapsto e
\]
-}
data Handler sig e = Handler
    { handlerClauses :: Map sig (Clause sig e)
    -- ^ The map of operation clauses \( \overline{c} \).
    , handlerFinal :: (Identifier, Exp sig e)
    -- ^ The final return clause \( x \mapsto e \).
    }

instance (Show sig, Show e) => Show (Handler sig e) where
    show (Handler cs (x, e)) =
        "(("
            ++ intercalate ", " [showClause op c | (op, c) <- Map.toList cs]
            ++ "), "
            ++ x
            ++ "->"
            ++ show e
            ++ ")"
      where
        showClause op (Clause m p b) =
            show op ++ show p ++ "(->" ++ arrowMode m ++ ")" ++ show b

        arrowMode Continue = "c"
        arrowMode Stop = "s"

{- | Performs substitution in a handler definition.

Used when reducing \( \text{handle } e \text{ with } h \).
-}
substHandler :: Identifier -> Val sig e -> Handler sig e -> Handler sig e
substHandler varName val (Handler clauses (finalVar, finalBody)) =
    let newClauses = substClause varName val <$> clauses
        newFinalBody =
            if finalVar == varName
                then finalBody
                else subst varName val finalBody
     in Handler newClauses (finalVar, newFinalBody)

-- | Performs substitution in a single clause.
substClause :: Identifier -> Val sig e -> Clause sig e -> Clause sig e
substClause varName val (Clause mode params body) =
    let newBody =
            if varName `elem` params
                then body
                else subst varName val body
     in Clause mode params newBody

{- | The set of expressions \( \mathbf{Exp} \).

Corresponds to Figure 2 and Figure 7.
-}
data Exp sig e
    = -- | Function application \( v_1 \ v_2 \).
      App (Val sig e) (Val sig e)
    | -- | Injection of values into expressions \( \text{return } v \).
      Ret (Val sig e)
    | -- | Monadic bind \( \text{do } x = e_1; e_2 \).
      Do Identifier (Exp sig e) (Exp sig e)
    | -- | Operation call \( op(\overline{v}) \).
      Magic sig [Val sig e]
    | -- | Handler application \( \text{handle } e \text{ with } h \).
      HandleWith (Exp sig e) (Handler sig e)
    | -- | Conditional \( \text{if } v \text{ then } e_1 \text{ else } e_2 \).
      If (Val sig e) (Exp sig e) (Exp sig e)
    | -- | Addition (primitive).
      Plus (Val sig e) (Val sig e)
    | -- | Subtraction (primitive).
      Minus (Val sig e) (Val sig e)
    | -- | Boolean AND (primitive).
      And (Val sig e) (Val sig e)
    | -- | Boolean OR (primitive).
      Or (Val sig e) (Val sig e)
    | -- | Zero test (primitive).
      IsZero (Val sig e)
    | -- | Successor (primitive).
      Suc (Val sig e)
    | -- | Predecessor (primitive).
      Pred (Val sig e)

instance (Show sig, Show e) => Show (Exp sig e) where
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

-- | Substitution on values: \( v[val/x] \).
substVal :: Identifier -> Val sig e -> Val sig e -> Val sig e
substVal varName val (IdentifierVal x) | x == varName = val
substVal varName val (LamVal x t body)
    | x == varName = LamVal x t body
    | otherwise = LamVal x t (subst varName val body)
substVal varName val v@(RecLamVal f t x body)
    | f == varName || x == varName = v
    | otherwise = RecLamVal f t x (subst varName val body)
substVal _ _ v = v

-- | Substitution on expressions: \( e[val/x] \).
subst :: Identifier -> Val sig e -> Exp sig e -> Exp sig e
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

-- | Multiple simultaneous substitutions.
substs :: [(Identifier, Val sig e)] -> Exp sig e -> Exp sig e
substs substitutions expr = foldr (\(var, val) e -> subst var val e) expr substitutions

{- | Abstract interface for operation signatures.

Defines the arity and type of operations in the signature \( \Sigma \).
-}
class Sig s where
    arity :: s -> ([ValType e], ValType e)





-- class (Sig sig) => ParsableSig sig where
--     bindOp :: String -> Maybe sig





{- | Abstract interface for the monadic semantics of operations.

This connects the syntactic operation calls to the underlying monad.
-}
class (Monad m, Sig sig) => MonSem m sig where
    {- | The interpretation function \( \text{run}_{op} \).

    Maps an operation and its arguments to a monadic computation.
    -}
    run :: sig -> [Val sig e] -> m (Val sig e)

{- | The "pure" reduction relation \( \to_p \).

Combines the rules from Figure 3 (Pure Reduction) and Figure 8 (Handlers).

Key Handler Rules:

*   **WITH-DO**: \( \text{handle } (\text{do } y=e_1; e_2) \text{ with } h \to \text{handle } e_1 \text{ with } (\dots) \)
*   **WITH-RET**: \( \text{handle } (\text{return } v) \text{ with } h \to \text{do } x = \text{return } v; e \)
*   **WITH-OP**: Dispatches to the appropriate clause based on the operation and mode.
-}
reducePure :: (Ord sig) => Exp sig e -> Maybe (Exp sig e)
reducePure e = case e of
    App (LamVal var _ body) arg -> Just $ subst var arg body
    App v@(RecLamVal f _ x body) arg -> Just $ (subst f v . subst x arg) body
    If (BoolVal True) bThen _ -> Just bThen
    If (BoolVal False) _ bElse -> Just bElse
    Plus (NatVal a) (NatVal b) -> Just $ Ret $ NatVal $ f a b
      where
        f x y
            | isZero x = y
            | otherwise = succ $ f x (pred y)
    Minus (NatVal a) (NatVal b) -> Just $ Ret $ NatVal $ f a b
      where
        f x y
            | isZero y = x
            | otherwise = f (pred x) (pred y)
    And (BoolVal a) (BoolVal b) -> Just $ Ret $ BoolVal (a && b)
    Or (BoolVal a) (BoolVal b) -> Just $ Ret $ BoolVal (a || b)
    IsZero (NatVal n) -> Just $ Ret $ BoolVal $ isZero n
    Suc (NatVal n) -> Just $ Ret $ NatVal $ succ n
    Pred (NatVal n) -> Just $ Ret $ NatVal $ pred n
    HandleWith (Do y e1 e2) h -> Just $ HandleWith e1 (h{handlerFinal = (y, HandleWith e2 h)})
    HandleWith (Ret v) (Handler _ (x, e')) -> Just $ Do x (Ret v) e'
    HandleWith (Magic op vs) (Handler cs (finalVar, finalBody)) ->
        case Map.lookup op cs of
            Just (Clause Continue params body) -> Just $ Do finalVar (substs (zip params vs) body) finalBody
            Just (Clause Stop params body) -> Just $ substs (zip params vs) body
            Nothing -> Just $ Do finalVar (Magic op vs) finalBody
    HandleWith e1 h -> HandleWith <$> reducePure e1 <*> pure h
    _ -> Nothing

{- | Implements the 'Valuable' interface for the language.

Maps \( \text{Ret } v \) to \( \text{Just } v \).
-}
instance Valuable (Exp sig e) (Val sig e) where
    toVal e = case e of
        Ret v -> Just v
        _ -> Nothing

{- | Implements the 'Reducible' interface for the language.

This defines the monadic reduction \( \to \) (Figure 4).

1.  __PURE__: If \( e \to_p e' \), then \( e \to \eta(e') \).
2.  __EFFECT__: \( op(\overline{v}) \to \text{map }(\text{return } [\,]) \ \text{run}_{op}(\overline{v}) \).
3.  __RET__: \( \text{do } x = \text{return } v; e \to \eta(e[v/x]) \).
4.  __DO__: \( \text{do } x = e_1; e_2 \to \text{map }(\text{do } x = [\,]; e_2) \ e_1 \).
-}
instance (MonSem m sig, Ord sig) => Reducible m (Exp sig e) (Val sig e) where
    reduce e =
        case reducePure e of
            Just e' -> Just (pure e')
            Nothing ->
                case e of
                    Magic op vs -> Just $ Ret <$> run op vs -- Rule (EFFECT)
                    Do var (Ret val) e2 -> Just $ pure $ subst var val e2 -- Rule (RET)
                    Do var e1 e2 ->
                        -- Rule (DO)
                        case reduce e1 of
                            Just m_e1' -> Just $ (\e1' -> Do var e1' e2) <$> m_e1'
                            Nothing -> Nothing
                    _ -> Nothing
