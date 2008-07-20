module Arith where
{
  data UnOp = Neg
            | Recip
            | Not
            deriving Show;
            
  tvUnOp :: UnOp -> TypedVal -> Result TypedVal EvalError;
  tvUnOp (Neg) (TVDouble x) = Result (TVDouble (negate x));
  tvUnOp (Recip) (TVDouble 0) = Fail DivByZero;
  tvUnOp (Recip) (TVDouble x) = Result (TVDouble (recip x));
  tvUnOp (Not) (TVBool x) = Result (TVBool (not x));
  tvUnOp _ _ = Fail TypeError;
            
  data Exp a = Unary UnOp (Exp a)
             deriving Show;
  
  data EvalError = DivByZero
                 deriving Show
}
