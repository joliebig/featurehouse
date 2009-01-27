module Arith where
{
  eval (Unary op exp) = mapResult (tvUnOp op) (eval exp)
}
