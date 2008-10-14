-- © 2006 Peter Thiemann
module CalcHistory where

import Char
import Prelude hiding (head, span, map)
import WASH.CGI.CGI hiding (div)

main = 
  run mainCGI

mainCGI = do
  callWithCurrentHistory doCalc ("0", Id)
  tell "Good Bye"

doCalc continueWith (display, fun) =
  standardQuery "Calculator" $ table $
  do dsp <- tr (td (textInputField (attr "value" display ## attr "name" "result")
		    ## attr "colspan" "4"))
     let button c = td (submit dsp (calcAction continueWith c fun)
				   (attr "value" [c] ## attr "name" ('k': [c])))
     tr (button '1' ## button '2' ## button '3' ## button '+')
     tr (button '4' ## button '5' ## button '6' ## button '-')
     tr (button '7' ## button '8' ## button '9' ## button '*')
     tr (button 'C' ## button '0' ## button '=' ## button '/')

calcAction continueWith c fun displayH
  | isDigit c = continueWith (dstr ++ [c], fun)
  | c == 'C'  = continueWith ("0", Id)
  | c == '='  = continueWith (show (apply fun (read dstr :: Integer)), Id) 
  | otherwise = continueWith ("0", getOp c fun (read dstr :: Integer))
  where dstr = value displayH

getOp '+' (Add z1) z2 = Add (z1 + z2)
getOp '+' _ z = Add z
getOp '-' (Sub z1) z2 = Sub (z1 - z2)
getOp '-' _ z = Sub z
getOp '*' (Mul z1) z2 = Mul (z1 * z2)
getOp '*' _ z = Mul z
getOp '/' (Div z1) z2 = Div (div z1 z2)
getOp '/' _ z = Div z

data CalcFunction
  = Add Integer
  | Sub Integer
  | Mul Integer
  | Div Integer
  | Id
  deriving (Read, Show)

apply Id z = z
apply (Add z1) z2 = (z1 + z2)
apply (Sub z1) z2 = (z1 - z2)
apply (Mul z1) z2 = (z1 * z2)
apply (Div z1) z2 = (div z1 z2)
