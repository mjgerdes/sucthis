-- | 

module SucThis.Eval where


import SucThis
import SucThis.Parse

unsafeRun :: String -> String
unsafeRun = f . parseProgram where
    f (Left error) = show error
    f (Right program) = show $ eval program


eval :: Expr -> Value
eval (EValue x) = x

eval (EPred (EValue (VNum Zero))) = VNum Zero
eval (EPred (EValue (VNum (Suc n)))) = eval $ EValue $ VNum n
eval (EPred e) = eval $ EPred $ EValue $ eval e

eval (EIs0 (EValue (VNum Zero))) = VTrue
eval (EIs0 (EValue (VNum _))) = VFalse
eval (EIs0 e) = eval $ EIs0 $ EValue $   eval e


eval (EIf (EValue VTrue) e1 e2) = eval e1
eval (EIf (EValue VFalse) e1 e2) = eval e2
eval (EIf e1 e2 e3) = eval $ EIf (EValue $ eval e1) e2 e3

