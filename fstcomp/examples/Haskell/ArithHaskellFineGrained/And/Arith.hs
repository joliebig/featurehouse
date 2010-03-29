module Arith where
{
  data BinOp = And
             deriving Show;
   
  tvBinOp (And) (TVBool x) (TVBool y)
    = Result (TVBool (x && y))
}
