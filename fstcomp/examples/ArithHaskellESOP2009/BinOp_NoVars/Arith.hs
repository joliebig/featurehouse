module Arith where
{
  eval (Binary op exp1 exp2)
    = zipResult (tvBinOp op) (eval exp1) (eval exp2)
}
