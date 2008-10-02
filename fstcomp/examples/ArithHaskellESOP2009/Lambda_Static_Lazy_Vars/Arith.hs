module Arith where
{
  data TypedVal = TVFun (Result TypedVal EvalError -> Result TypedVal EvalError);	-- Lazy and Static Lambdas

  -- lazy-static
  eval env (Lam x exp)
    = Result $ TVFun (\ val -> eval (addToEnv env x val) exp);
  eval env (App exp1 exp2)
    = case (eval env exp1, eval env exp2) of
          { (Result (TVFun f), arg) -> f arg;
            (Result _, _) -> Fail ApplicationError;
           (Fail err, _) -> Fail err}   
}
