module Arith where
{
  data UnOp = Neg
            deriving Show;
            
  tvUnOp (Neg) (TVDouble x) = Result (TVDouble (negate x))
}
