module Arith where
{
  data BinOp = Div
             deriving Show;

  data EvalError = DivByZero
                 deriving Show;
   
  tvBinOp (Div) (TVDouble x) (TVDouble 0) = Fail DivByZero;
  tvBinOp (Div) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x / y))

}
