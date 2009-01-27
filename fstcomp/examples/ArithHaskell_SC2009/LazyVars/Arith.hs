module Arith where
{
  eval :: Env (Result TypedVal EvalError) -> Exp TypedVal -> Result TypedVal EvalError;

  eval env (Let defs exp) = eval newEnv exp
    where { (names, exps) = unzip defs;
            vals = map (eval env) exps;
            newEnv = addToEnvN env (zip names vals)}
}
