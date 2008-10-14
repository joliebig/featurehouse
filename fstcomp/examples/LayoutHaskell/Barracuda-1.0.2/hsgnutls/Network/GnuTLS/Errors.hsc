module Network.GnuTLS.Errors where

#include <gnutls/gnutls.h>

import Foreign.C.Types
--import Network.GnuTLS.GnuTLSMonad

errorShortMemory :: CInt
errorShortMemory = #{const GNUTLS_E_SHORT_MEMORY_BUFFER}
