module Arith where
{
  data BinOp = DummyBinOp
             deriving Show;
   
  tvBinOp _ _ _ = Fail TypeError;
  tvBinOp :: BinOp -> TypedVal -> TypedVal -> Result TypedVal EvalError;
   
  data Exp a = Binary BinOp (Exp a) (Exp a)
             deriving Show;
  
  data EvalError = DivByZero
                 deriving Show
}
