module Arith where
{
  data TypedVal = TVFun (Env (Result TypedVal EvalError) ->							-- Lazy and Dynamic Lambdas
                           Result TypedVal EvalError -> Result TypedVal EvalError);
                           
  -- lazy-dynamic
  eval _ (Lam x exp) = Result $ TVFun f
    where { f env val
              = case eval (addToEnv env x val) exp of
                    { Result (TVFun g) -> Result $ TVFun h
                        where { h env2 val2 = g (addToEnv env2 x val) val2};
                      r -> r}};
  eval env (App exp1 exp2)
    = case (eval env exp1, eval env exp2) of
          { (Result (TVFun f), arg) -> f env arg;
            (Result _, _) -> Fail ApplicationError;
            (Fail err, _) -> Fail err}
   
}
