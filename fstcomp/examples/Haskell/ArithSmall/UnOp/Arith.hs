module Arith where
{
  data UnOp = DummyUnOp
            deriving Show;

  tvUnOp _ _ = Fail TypeError;            
  tvUnOp :: UnOp -> TypedVal -> Result TypedVal EvalError;
            
  data Exp a = Unary UnOp (Exp a)
             deriving Show;
  
  data EvalError = DivByZero
                 deriving Show
}
