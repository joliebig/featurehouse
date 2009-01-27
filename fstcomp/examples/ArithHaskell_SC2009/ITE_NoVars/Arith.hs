module Arith where
{

  -- ITE 
  eval (ITE exp1 exp2 exp3) = case eval exp1 of
          { Result (TVDouble x)
              | x >= 0 -> eval exp2
              | otherwise -> eval exp3;
            Result _ -> Fail TypeError;
            Fail err -> Fail err}
}
