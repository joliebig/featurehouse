module Arith where
{
  data BinOp = Mul
             deriving Show;
   
  tvBinOp (Mul) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x * y))
}
