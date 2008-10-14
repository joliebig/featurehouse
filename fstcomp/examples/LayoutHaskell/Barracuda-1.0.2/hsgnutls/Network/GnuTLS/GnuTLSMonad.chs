
module Network.GnuTLS.GnuTLSMonad(
	GnuTLSError(..),
	GnuTLSMonad,
	getDescription,
	withGnuTLS,
	gnuTLSCheckBool 
	) where

import Network.GnuTLS.GnuTLS
--import Data.ByteString.Base
import System.IO.Unsafe(unsafePerformIO)
import Control.Exception(finally)
import Control.Monad(unless)
import Control.Monad.Error
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

#include <gnutls/gnutls.h>

{#context prefix="gnutls"#}

withGnuTLS :: IO a -> IO a
withGnuTLS act = do
	res <- {#call unsafe gnutls_global_init#}
	unless (res==0) (fail "couldn't initialize gnutls")
	act `finally` {#call unsafe gnutls_global_deinit#}

data GnuTLSError = GnuTLSError CInt deriving (Eq)

instance Error GnuTLSError where
	noMsg = GnuTLSError 0

getDescription :: GnuTLSError -> String
getDescription (GnuTLSError err) = unsafePerformIO $ {#call gnutls_strerror#} err >>= peekCString

instance Show GnuTLSError where
	show err = getDescription err

type GnuTLSMonad a = Either GnuTLSError a

checkError :: CInt -> GnuTLSMonad ()
checkError 0 = Right ()
checkError i = Left (GnuTLSError i)

gnuTLSCheckBool :: CInt -> GnuTLSMonad Bool
gnuTLSCheckBool 0 = return False
gnuTLSCheckBool 1 = return True
gnuTLSCheckBool i = Left (GnuTLSError i)
