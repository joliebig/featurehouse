module Arith where
{
  data BinOp = Add 
             deriving Show;
   
  tvBinOp (Add) (TVString s) (TVString t)
    = Result (TVString (s ++ t));
  tvBinOp (Add) (TVString s) (TVDouble y)
    = Result (TVString (s ++ show y));
  tvBinOp (Add) (TVDouble x) (TVString t)
    = Result (TVString (show x ++ t));
  tvBinOp (Add) (TVDouble x) (TVDouble y)
    = Result (TVDouble (x + y))
}
