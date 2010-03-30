module Arith where
{
  -- NoVars
  evalExp :: Exp TypedVal -> Result TypedVal EvalError;
  evalExp exp = eval emptyEnv exp

  eval :: Env (Result TypedVal EvalError) -> Env TypedVal -> Exp TypedVal -> Result TypedVal EvalError;
  eval _ (Const x) = Result x;
  eval env (Var name)
    = case lookupEnv env name of
          { Just x -> Result x;
            Nothing -> Fail UndefVarError};
  eval env (Let defs exp)
    = case liftResult (map (eval env) exps) of
          { Result vals -> eval newEnv exp
              where { newEnv = addToEnvN env (zip names vals)};
            Fail e -> Fail e}
    where { (names, exps) = unzip defs};
  eval env (Let defs exp) = eval newEnv exp
    where { (names, exps) = unzip defs;
            vals = map (eval env) exps;
            newEnv = addToEnvN env (zip names vals)};
  
  -- ITE 
  eval env (ITE exp1 exp2 exp3) = case eval env exp1 of
          { Result (TVDouble x)
              | x >= 0 -> eval env exp2
              | otherwise -> eval env exp3;
            Result _ -> Fail TypeError;
            Fail err -> Fail err};
  
  -- eval bei Lambdas ----------------------------------------------------------------
  
  -- NoVars, lambdas-lazy
  eval (Lam x exp) = Result $ TVFun (\ _ -> eval exp);
  eval (App exp1 exp2)
    = case (eval exp1, eval exp2) of
          { (Result (TVFun f), arg) -> f arg;
            (Result _, _) -> Fail ApplicationError;
            (Fail err, _) -> Fail err};
                
  -- NoVars, lambdas-strict
  eval (Lam x exp) = Result $ TVFun (\ _ -> eval exp);
  eval (App exp1 exp2)
    = case (eval exp1, eval exp2) of
          { (Result (TVFun f), Result arg) -> f arg;
            (Result (TVFun _), Fail err) -> Fail err;
            (Result _, _) -> Fail ApplicationError;
            (Fail err, _) -> Fail err};
                
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
            (Fail err, _) -> Fail err};
                           
  -- lazy-static
  eval env (Lam x exp)
    = Result $ TVFun (\ val -> eval (addToEnv env x val) exp);
  eval env (App exp1 exp2)
    = case (eval env exp1, eval env exp2) of
          { (Result (TVFun f), arg) -> f arg;
            (Result _, _) -> Fail ApplicationError;
           (Fail err, _) -> Fail err};
    
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
            (Fail err, _) -> Fail err};
                
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
