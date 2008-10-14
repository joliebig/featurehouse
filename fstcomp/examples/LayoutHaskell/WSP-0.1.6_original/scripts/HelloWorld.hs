-- © 2001-2005 Peter Thiemann
module HelloWorld where

import Prelude hiding (head, span, div, map)
import WASH.CGI.CGI

mainCGI :: CGI ()
mainCGI =
  standardQuery "Hello World" empty
