module Arith where
{
  data TypedVal = TVFun (Env TypedVal -> TypedVal -> Result TypedVal EvalError);		-- Strict and Dynamic Lambdas
  
  -- strict dynamic
  eval _ (Lam x exp) = Result $ TVFun f
    where { f env val
              = case eval (addToEnv env x val) exp of
                    { Result (TVFun g) -> Result $ TVFun h
                        where { h env2 val2 = g (addToEnv env2 x val) val2};
                      r -> r}};
  eval env (App exp1 exp2)
    = case (eval env exp1, eval env exp2) of
          { (Result (TVFun f), Result arg) -> f env arg;
            (Result (TVFun _), Fail err) -> Fail err;
            (Result _, _) -> Fail ApplicationError;
            (Fail err, _) -> Fail err}

}
