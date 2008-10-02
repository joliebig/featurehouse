module Arith where
{
  data TypedVal = TVFun (Result TypedVal EvalError -> Result TypedVal EvalError);	-- NoVars, Lazy Lambdas
                           
  -- NoVars, lambdas-lazy
  eval (Lam x exp) = Result $ TVFun (\ _ -> eval exp);
  eval (App exp1 exp2)
    = case (eval exp1, eval exp2) of
          { (Result (TVFun f), arg) -> f arg;
            (Result _, _) -> Fail ApplicationError;
            (Fail err, _) -> Fail err}
   
}
