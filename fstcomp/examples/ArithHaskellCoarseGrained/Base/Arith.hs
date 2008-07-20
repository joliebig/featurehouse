module Arith where
{
  -- In allen Features vorhanden -----------------------------------------------------------
   
  data Result a err = Result a
                    | Fail err
                    deriving Show;
   
  mapResult :: (a -> Result b err) -> Result a err -> Result b err;
  mapResult f (Result x) = f x;
  mapResult f (Fail e) = Fail e;
   
  zipResult ::
              (a -> b -> Result c err) ->
                Result a err -> Result b err -> Result c err;
  zipResult f (Result x) (Result y) = f x y;
  zipResult _ (Fail e) _ = Fail e;
  zipResult _ _ (Fail e) = Fail e;
   
  liftResult :: [Result a err] -> Result [a] err;
  liftResult [] = Result [];
  liftResult (Result x : rest)
    = case liftResult rest of
          { Result xs -> Result (x : xs);
            Fail e -> Fail e};
  liftResult (Fail e : _) = Fail e;

  -- Vars
  data Env a = Env [(String, a)]
             deriving Show;
   
  lookupEnv :: Env a -> String -> Maybe a;
  lookupEnv (Env env) name = lookup name env;
   
  emptyEnv :: Env a;
  emptyEnv = Env [];
   
  addToEnv :: Env a -> String -> a -> Env a;
  addToEnv (Env env) name val = Env ((name, val) : env);
   
  addToEnvN :: Env a -> [(String, a)] -> Env a;
  addToEnvN (Env env) newdefs = Env (newdefs ++ env);
   
  data Exp a = Const a
             | Var String						-- Vars
             | Let [(String, Exp a)] (Exp a)	-- Vars
             | ITE (Exp a) (Exp a) (Exp a)		-- ITE
             | Lam String (Exp a)				-- Lambda
             | App (Exp a) (Exp a)				-- Lambda
             deriving Show;
  
  -- TODO MRO: DivByZero bei BinOps ODER UnOps
  data EvalError = Overflow
                 | TypeError
                 | UndefVarError				-- Vars
                 | ApplicationError				-- Lambda
                 deriving Show;
  
  data TypedVal = TVString String
                | TVDouble Double
                | TVBool Bool
                | TVFun (Env (Result TypedVal EvalError) ->							-- Lazy and Dynamic Lambdas
                           Result TypedVal EvalError -> Result TypedVal EvalError)
                | TVFun (Result TypedVal EvalError -> Result TypedVal EvalError)	-- Lazy and Static Lambdas
                | TVFun (Env TypedVal -> TypedVal -> Result TypedVal EvalError)		-- Strict and Dynamic Lambdas
                | TVFun (TypedVal -> Result TypedVal EvalError);					-- Strict and Static Lambdas
   
  instance Show TypedVal where
          { show (TVString s) = show s; 
            show (TVDouble d) = show d;
            show (TVBool b) = show b;
            show (TVFun _) = "<<function>>"		-- Lambdas
          }
}
