module Arith where
{
  data Exp a = Lam String (Exp a)				-- Lambda
             | App (Exp a) (Exp a)				-- Lambda
             deriving Show;
  
  -- TODO MRO: DivByZero bei BinOps ODER UnOps
  data EvalError = ApplicationError				-- Lambda
                 deriving Show;
  
  instance Show TypedVal where
          { show (TVFun _) = "<<function>>"		-- Lambdas
          }
}
