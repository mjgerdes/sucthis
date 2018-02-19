-- | 

module Main where
import Prelude

import SucThis
import SucThis.Parse
import SucThis.Type
import SucThis.Eval



typeCheck :: String -> Either TypeError TypedExpr
-- Processes a program given as a string only up to type checking,
-- i.e. tries to parse and then type check, does not evaluate.
typeCheck p = check $ parseProgram p where
    check (Left err) = Left $ show err
    check (Right exprTree) = infer exprTree


run :: String -> String
-- Does everything, parse, typecheck and evaluate.
run = checkTypeErrors . checkParseErrors . parseProgram where
    checkParseErrors (Left error) = Left $ show error
    checkParseErrors (Right program) = infer program
    checkTypeErrors (Left errorString) = errorString
    checkTypeErrors (Right safeProgram) = show $ eval $ exprOf safeProgram

main :: IO ()
main = interact run
  



    
