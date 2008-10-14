-- |
-- Maintainer: Stephan Friedrichs, Henning Guenther
--
-- This module provides a bundle of helper-functions used in several
-- places of the software.
module Barracuda.Utils where

import Prelude hiding (catch)
import Control.Exception (catch)
import Data.Char
import System.Directory
import Network.Socket
import System.FilePath
import System.IO.Unsafe

-- | Reads a hostname to a 'String', if possible.
readHost :: String -> Maybe HostAddress
readHost str = unsafePerformIO $
	(inet_addr str >>= return.Just) `catch` (\err -> return Nothing)

-- | Does the same as 'readHost', only that it throws an error on failure.
readHost' :: String -> HostAddress
readHost' str = maybe (error $ "Couldn't parse address "++str) id (readHost str)

-- | Parses a 'String' into a 'SockAddr'.
readSockAddr :: PortNumber -- ^ The default portnumber to use, if none is given in the 'String'.
	-> String          -- ^ This is where the 'SockAddr' is parsed from.
	-> Maybe SockAddr
readSockAddr def str = do
	let (ip,rest) = break (==':') str
	host <- readHost ip
	case rest of
		[]       -> return $ SockAddrInet def host
		':':port -> if all isDigit port
			then return $ SockAddrInet (fromIntegral $ read port) host
			else fail "couldn't parse port"
		_        -> fail "':' expected"

-- | The same as 'readSockAddr', only that an error is thrown on failure.
readSockAddr' :: PortNumber -> String -> SockAddr
readSockAddr' def str = maybe (error $ "Couldn't parse address "++str) id (readSockAddr def str)

-- | Converts a 'HostAddress' to a 'String'.
showHost :: HostAddress -> String
showHost = unsafePerformIO.inet_ntoa

-- | Returns the directory where the configuration is saved.
getConfigDirectory :: IO FilePath
getConfigDirectory = getHomeDirectory >>= return.(</> ".barracuda")

-- | The root directrory of the certificate cache.
getCertificateDirectory :: IO FilePath
getCertificateDirectory = getConfigDirectory >>= return.(</> "certificates")

-- | A file saving the most recently used certificate.
getLastCertificate :: IO FilePath
getLastCertificate = getConfigDirectory >>= return.(</> "last-cert")

-- | A file saving the most recently used private key.
getLastKey :: IO FilePath
getLastKey = getConfigDirectory >>= return.(</> "last-key")

