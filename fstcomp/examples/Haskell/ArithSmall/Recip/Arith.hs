module Arith where
{
  data UnOp = Recip
            deriving Show;

  data EvalError = DivByZero
                 deriving Show;
            
  tvUnOp (Recip) (TVDouble 0) = Fail DivByZero;
  tvUnOp (Recip) (TVDouble x) = Result (TVDouble (recip x))
}
