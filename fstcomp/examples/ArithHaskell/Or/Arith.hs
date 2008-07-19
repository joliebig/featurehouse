module Arith where
{
  data BinOp = Or
             deriving Show;
   
  tvBinOp (Or) (TVBool x) (TVBool y)
    = Result (TVBool (x || y))
}
