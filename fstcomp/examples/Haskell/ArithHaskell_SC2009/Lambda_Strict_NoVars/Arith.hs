module Arith where
{
  data TypedVal = TVFun (TypedVal -> Result TypedVal EvalError);						-- NoVars, Strict Lambdas
  
    -- NoVars, lambdas-strict
  eval (Lam x exp) = Result $ TVFun (\ _ -> eval exp);
  eval (App exp1 exp2)
    = case (eval exp1, eval exp2) of
          { (Result (TVFun f), Result arg) -> f arg;
            (Result (TVFun _), Fail err) -> Fail err;
            (Result _, _) -> Fail ApplicationError;
            (Fail err, _) -> Fail err}
}
