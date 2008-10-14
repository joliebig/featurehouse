module Network.GnuTLS.IOWrap() where

import Foreign
import Network.GnuTLS.GnuTLS

--------------------------------------------------------------------------------
--------------------------   Pull/Push Wrapper   -------------------------------
--------------------------------------------------------------------------------

foreign export ccall gnutls_io_wrap_h :: 
  StablePtr Transport -> Transport

gnutls_io_wrap_h :: StablePtr Transport -> Transport
gnutls_io_wrap_h sptr p l e = do
  c <- deRefStablePtr sptr
  c p l e

