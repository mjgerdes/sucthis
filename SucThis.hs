-- | 

module SucThis where

data Expr =
  EValue Value |
  EPred Expr |
  EIs0 Expr |
  EIf Expr Expr Expr 
  deriving (Show, Eq, Ord)

data Value = VTrue | VFalse | VNum Nat
  deriving (Show, Eq, Ord)


data Nat = Zero | Suc Nat
  deriving (Show, Eq, Ord)

nullexpr = EValue $ VNum $ Zero
