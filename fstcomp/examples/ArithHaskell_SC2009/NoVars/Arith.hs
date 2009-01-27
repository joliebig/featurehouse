module Arith where
{
  eval :: Exp TypedVal -> Result TypedVal EvalError;
  eval (Const x) = Result x;
  
  -- NoVars
  evalExp :: Exp TypedVal -> Result TypedVal EvalError;
  evalExp exp = eval exp
}
