module SST.Lib where

infixl 4 :@
infixr 3 :->

type Symb = String

-- Term
data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
    deriving (Eq, Show)

-- Type
data Type = TVar Symb | Type :-> Type
    deriving (Eq, Show)

-- Context
newtype Env = Env [(Symb,Type)]
    deriving (Eq, Show)

-- Substitution
newtype SubsTy = SubsTy [(Symb, Type)]
    deriving (Eq, Show)
