-- © 2005 Peter Thiemann
module CGICounter where

import Prelude hiding (map, span, head, div)
import WASH.CGI.CGI

mainCGI :: CGI ()
mainCGI = do
  counter 0

counter n =
  standardQuery "Counter" $
  do text "Current counter value "
     text (show n)
     br empty
     submit0 (counter (n + 1)) (fieldVALUE "Increment")
     submit0 (counter (n - 1)) (fieldVALUE "Decrement")
