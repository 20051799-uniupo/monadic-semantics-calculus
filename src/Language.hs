module Language
  ( Val (..),
    Exp (..),
    Peano (..),
    subst,
    add,
    sub
  )
where

type Identifier = String

data Peano = Zero | Succ Peano deriving (Show)

add :: Peano -> Peano -> Peano
add Zero n = n
add (Succ m) n = Succ (add m n)

sub :: Peano -> Peano -> Peano
sub _ Zero = Zero
sub Zero _ = Zero
sub (Succ m) (Succ n) = sub m n

data Val sig
  = NumVal Peano
  | BoolVal Bool
  | LamVal Identifier (Exp sig)
  | RecLamVal Identifier Identifier (Exp sig)
  | IdentifierVal Identifier
  deriving (Show)

data Exp sig
  = App (Val sig) (Val sig)
  | Ret (Val sig)
  | Do Identifier (Exp sig) (Exp sig)
  | Magic sig [Val sig]
  | If (Val sig) (Exp sig) (Exp sig)
  | Plus (Val sig) (Val sig)
  | Minus (Val sig) (Val sig)
  | And (Val sig) (Val sig)
  | Or (Val sig) (Val sig)
  | IsZero (Val sig)
  deriving (Show)

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
  If p bThen bElse -> If (substVal varName val p) (subst varName val bThen) (subst varName val bElse)
  Plus (a) (b) -> Plus (substVal varName val a) (substVal varName val b)
  Minus (a) (b) -> Minus (substVal varName val a) (substVal varName val b)
  And (a) (b) -> And (substVal varName val a) (substVal varName val b)
  Or (a) (b) -> Or (substVal varName val a) (substVal varName val b)
  IsZero (n) -> IsZero (substVal varName val n)
