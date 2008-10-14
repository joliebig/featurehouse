-- © 2001-2005 Peter Thiemann
module Multiplication where

import Prelude hiding (head, span, div, map)
import WASH.CGI.CGI
import Random
import qualified WASH.CGI.Persistent2 as P

mainCGI :: CGI ()
mainCGI =
 standardQuery "Multiplication Drill" $
  <p>Hi there! What's your name?
     <input type="text" name="nameF" />
     <input type="submit"
            WASH:callback="mdrill" WASH:parms="nameF" />
  </p>

mdrill :: NonEmpty -> CGI ()
mdrill nameNE =
  let name = unNonEmpty nameNE in
  standardQuery "Multiplication" $
  <#>
    <p>Hello <%= name %>!</p>
    <p>Let's exercise some multiplication!</p>
    <p>Give me a multiplier 
      <input type="text" value="2" name="mpyF" />
    </p>
    <p>Number of exercises 
      <input type="text" value="10" name="rptF" />
    </p>
    <input type="submit" value="GO!"
	   WASH:callback="firstExercise name" WASH:parms="mpyF,rptF" />
  </#>

firstExercise :: String -> (Int, Int) -> CGI ()
firstExercise name (mpy, rpt) = 
  runExercises 1 [] []
  where
-- 
    runExercises nr successes failures =
      if nr > rpt then 
        finalReport
      else
        do factor <- io (randomRIO (0,12))
	   standardQuery ("Question " ++ show nr ++ " of " ++ show rpt) $
	     do text (show factor ++ " * " ++ show mpy ++ " = ")
		activate (checkAnswer factor) inputField empty
      where
	checkAnswer factor answer =
          let correct = answer == factor * mpy 
	      message = if correct then "correct! " else "wrong! "
	      continue F0 = if correct then 
	      		      runExercises (nr+1) (factor:successes) failures
			    else
			      runExercises (nr+1) successes (factor:failures)
	  in standardQuery ("Answer " ++ show nr ++ " of " ++ show rpt) $
	  do p (text (show factor ++ " * " ++ show mpy ++ " = " ++ show (factor * mpy)))
	     text ("Your answer " ++ show answer ++ " was " ++ message)
	     submit F0 continue (attr "value" "CONTINUE")
-- 
	finalReport =
	  let lenSucc = length successes 
	      pItem (m, l, r) = li (text ("Multiplier " ++ show m ++
	                              " : " ++ show l ++
				      " correct out of " ++ show r))
	  in
	  do initialHandle <- P.init ("multi-" ++ name) []
	     currentHandle <- P.add initialHandle (mpy, lenSucc, rpt)
	     hiScores <- P.get currentHandle
	     standardQuery "Final Report" $ 
	       <#>
		 <p>Here are your recent scores.</p>
		 <ul><% mapM_ pItem hiScores %></ul>
	       </#>

