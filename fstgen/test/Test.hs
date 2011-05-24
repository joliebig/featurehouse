module Arith where
{
  class (Monad m) => GraphM m gr where
          {  
            emptyM :: m (gr a b)
         };

  data BinOp = Add
             | Sub
             | Mul
             | Div
             | And
             | Or
             deriving Show;
   
  data UnOp = Neg
            | Recip
            | Not
            deriving Show;
            
  tvBinOp ::
            BinOp -> TypedVal -> TypedVal -> Result TypedVal EvalError;
  tvBinOp (Add) (TVString s) (TVString t)
    = Result (TVString (s ++ t));
  tvBinOp (Add) (TVString s) (TVDouble y)
    = Result (TVString (s ++ show y));
  tvBinOp (Add) (TVDouble x) (TVString t)
    = Result (TVString (show x ++ t));
  tvBinOp (Add) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x + y));
  tvBinOp (Sub) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x - y));
  tvBinOp (Mul) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x * y));
  tvBinOp (Div) (TVDouble x) (TVDouble 0) = Fail DivByZero;
  tvBinOp (Div) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x / y));
  tvBinOp (And) (TVBool x) (TVBool y)
    = Result (TVBool (x && y));
  tvBinOp (Or) (TVBool x) (TVBool y)
    = Result (TVBool (x || y));
  tvBinOp _ _ _ = Fail TypeError;
   
  tvUnOp :: UnOp -> TypedVal -> Result TypedVal EvalError;
  tvUnOp (Neg) (TVDouble x) = Result (TVDouble (negate x));
  tvUnOp (Recip) (TVDouble 0) = Fail DivByZero;
  tvUnOp (Recip) (TVDouble x) = Result (TVDouble (recip x));
  tvUnOp (Not) (TVBool x) = Result (TVBool (not x));
  tvUnOp _ _ = Fail TypeError;
            
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
             | Binary BinOp (Exp a) (Exp a)
             | Unary UnOp (Exp a)
             | Let [(String, Exp a)] (Exp a)	-- Vars
             | ITE (Exp a) (Exp a) (Exp a)		-- ITE
             | Lam String (Exp a)				-- Lambda
             | App (Exp a) (Exp a)				-- Lambda
             deriving Show;
  
  -- TODO MRO: DivByZero bei BinOps ODER UnOps
  data EvalError = DivByZero
                 | Overflow
                 | TypeError
                 | UndefVarError				-- Vars
                 | ApplicationError				-- Lambda
                 deriving Show;
  
  data TypedVal = TVString String
                | TVDouble Double
                | TVBool Bool
                | TVFun (Result TypedVal EvalError -> Result TypedVal EvalError)	-- NoVars, Lazy Lambdas
                | TVFun (TypedVal -> Result TypedVal EvalError)						-- NoVars, Strict Lambdas
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
          };

  -- ---------------------------------------------------------------------------------------------------
  -- NoVars
  --eval :: Exp TypedVal -> Result TypedVal EvalError;
  --eval (Const x) = Result x;
  --eval (Binary op exp1 exp2)
  --  = zipResult (tvBinOp op) (eval exp1) (eval exp2);
  --eval (Unary op exp) = mapResult (tvUnOp op) (eval exp);
  
  -- StrictVars
  --eval :: Env TypedVal -> Exp TypedVal -> Result TypedVal EvalError;
  --eval _ (Const x) = Result x;
  --eval env (Var name)
  --  = case lookupEnv env name of
  --        { Just x -> Result x;
  --          Nothing -> Fail UndefVarError};
  --eval env (Binary op exp1 exp2)
  --  = zipResult (tvBinOp op) (eval env exp1) (eval env exp2);
  --eval env (Unary op exp) = mapResult (tvUnOp op) (eval env exp);
  --eval env (Let defs exp)
  --  = case liftResult (map (eval env) exps) of
  --        { Result vals -> eval newEnv exp
  --            where { newEnv = addToEnvN env (zip names vals)};
  --          Fail e -> Fail e}
  --  where { (names, exps) = unzip defs};
   
  -- LazyVars
  --eval :: Env (Result TypedVal EvalError) -> Exp TypedVal -> Result TypedVal EvalError;
  --eval _ (Const x) = Result x;
  --eval env (Var name) = case lookupEnv env name of
  --        { Just x -> x;
  --          Nothing -> Fail UndefVarError};
  --eval env (Binary op exp1 exp2)
  --  = zipResult (tvBinOp op) (eval env exp1) (eval env exp2);
  --eval env (Unary op exp) = mapResult (tvUnOp op) (eval env exp);
  --eval env (Let defs exp) = eval newEnv exp
  --  where { (names, exps) = unzip defs;
  --          vals = map (eval env) exps;
  --          newEnv = addToEnvN env (zip names vals)};
  -- ---------------------------------------------------------------------------------------------------
  
  eval :: Env (Result TypedVal EvalError) -> Env TypedVal -> Exp TypedVal -> Result TypedVal EvalError;
  eval _ (Const x) = Result x;
  eval env (Var name)
    = case lookupEnv env name of
          { Just x -> Result x;
            Nothing -> Fail UndefVarError};
  eval env (Binary op exp1 exp2)
    = zipResult (tvBinOp op) (eval env exp1) (eval env exp2);
  eval env (Unary op exp) = mapResult (tvUnOp op) (eval env exp);
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
            (Fail err, _) -> Fail err};
  
  -- NoVars
  --evalExp :: Exp TypedVal -> Result TypedVal EvalError;
  --evalExp exp = eval exp;
  
  evalExp :: Exp TypedVal -> Result TypedVal EvalError;
  evalExp exp = eval emptyEnv exp;
  
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
  liftResult (Fail e : _) = Fail e
}
