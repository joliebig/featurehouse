module Arith where
{
  data TypedVal =  TVFun (TypedVal -> Result TypedVal EvalError);					-- Strict and Static Lambdas

  -- strict-static
  eval env (Lam x exp)
    = Result $ TVFun (\ val -> eval (addToEnv env x val) exp);
  eval env (App exp1 exp2)
    = case (eval env exp1, eval env exp2) of
          { (Result (TVFun f), Result arg) -> f arg;
            (Result (TVFun _), Fail err) -> Fail err;
            (Result _, _) -> Fail ApplicationError;
            (Fail err, _) -> Fail err}
   
}
