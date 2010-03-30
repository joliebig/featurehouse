module Arith where
{
  data BinOp = Sub
             deriving Show;
   
  tvBinOp (Sub) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x - y))
}
