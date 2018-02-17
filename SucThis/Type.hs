-- | 

module SucThis.Type where

import SucThis
import SucThis.Parse
import SucThis.Eval

import Data.Maybe
import Data.Either

data TypedExpr = TypedExpr {
    exprOf :: Expr,
    typeOf :: Type}
  deriving (Show, Eq, Ord)

data Type = TBool | TNat | TBottom
  deriving (Show, Eq, Ord)

type TypeError = String

errorMsg :: Maybe Expr -> Expr -> Type -> Type -> Either TypeError TypedExpr
errorMsg outerExpr expr expected actual = Left $ unlines $ (outerMsg outerExpr) ++ msgs
    where outerMsg Nothing = []
          outerMsg (Just e) = ["Could not deduce type of expression: " ++ (show e)]
          msgs = ["Type of '" ++ (show expr) ++ "' does not match.",
                  "  expected: " ++ (show expected),
                  "  actual type: " ++ (show actual)]


typeCheck :: String -> Either TypeError TypedExpr
typeCheck p = check $ parseProgram p where
    check (Left err) = Left $ show err
    check (Right exprTree) = infer exprTree

typedExpr :: Expr -> Type -> Either TypeError TypedExpr
typedExpr e1 t1 = Right $ TypedExpr e1 t1

inferOrBot :: Expr -> TypedExpr
inferOrBot e = fromRight (TypedExpr e TBottom) $ infer e

infer :: Expr -> Either TypeError TypedExpr

-- boolean rules
infer e1@(EValue VTrue) = typedExpr e1 TBool
infer e1@(EValue VFalse) = typedExpr e1 TBool

-- natural number rules
infer e1@(EValue (VNum Zero)) = typedExpr e1 TNat
infer e1@(EValue (VNum (Suc n)))
      | t2 == t1 = typedExpr e1 t1
      | otherwise = errorMsg Nothing e2 t1 t2
      where  e2 = (EValue $ VNum n)
             t2 = typeOf $ inferOrBot e2
             t1 = TNat

-- inference rules for complex expressions
-- pred
infer e1@(EPred e2)
    | t1 == t2 = typedExpr e1 t1
    | otherwise = errorMsg (Just e1) e2 t1 t2
      where t1 = TNat
            t2 = typeOf $ inferOrBot e2 

-- is0
infer e1@(EIs0 e2)
    | t2 == TNat = typedExpr e1 t1
    | otherwise = errorMsg (Just e1) e2 TNat t2
      where t1 = TBool
            t2 = typeOf $ inferOrBot e2

-- if then elseinfer
infer e1@(EIf econd ethen eelse)
    | and [tcond == TBool, tthen == telse] = typedExpr e1 tthen
    | tcond /= TBool = errorMsg (Just e1) econd TBool tcond
    | tthen /= telse = errorMsg (Just e1) ethen telse tthen
      where tcond = typeOf $ inferOrBot econd
            tthen = typeOf $ inferOrBot ethen
            telse = typeOf $ inferOrBot eelse
