-- | 

module SucThis.Parse where

import SucThis

import Text.Parsec
import Text.Parsec.String

loadProgram :: String -> IO (Either ParseError Expr)
loadProgram filename = readFile filename >>= return . parse expr filename

parseProgram :: String -> Either ParseError Expr
parseProgram w = parse expr "input" w

vtrue :: Parsec String () Value
vtrue = string "true" >> return VTrue

vfalse :: Parsec String () Value
vfalse = string "false" >> return VFalse

natzero :: Parsec String () Nat
natzero = string "0" >> return Zero

suc = string "suc"

natsuc :: Parsec String () Nat
natsuc = do
  suc
  space
  rest <- nat
  return $ Suc rest


nat :: Parsec String () Nat
nat = natzero <|> natsuc

vnum :: Parsec String () Value
vnum = nat >>= return . VNum

value :: Parsec String () Value
value = vtrue <|> vfalse <|> vnum


tpred = string "pred" 
tif = string "if"
tthen = string "then"
telse = string "else"
tis0 = string "is0"

epred :: Parsec String () Expr
epred = do
    tpred
    space
    innerExpr <- expr
    return $ EPred innerExpr

eis0 :: Parsec String () Expr
eis0 = do
    tis0
    space
    innerExpr <- expr
    return $ EIs0 innerExpr

eif :: Parsec String () Expr
eif = do
    tif
    space
    condExpr <- expr
    space
    tthen
    space
    trueExpr <- expr
    space
    telse
    space
    falseExpr <- expr
    return $ EIf condExpr trueExpr falseExpr

evalue = value >>= return . EValue
    
expr :: Parsec String () Expr
expr = evalue <|> epred <|> (try eis0) <|> eif

p1 = "0"
p2 = "suc suc suc 0"
p3 = "if is0 pred suc 0 then true else false"
p4 = "pred true"
p5 = "if true then 0 else false"
