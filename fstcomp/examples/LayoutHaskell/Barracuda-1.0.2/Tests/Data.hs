module Tests.Data where

import Data.ByteString as BS hiding (map, putStrLn)
import Data.Time
import System.FilePath
import Network.AdHoc.Channel
import Network.AdHoc.Signature
import Network.AdHoc.Message
import Network.AdHoc.UserID
import Network.Socket
import Network.GnuTLS
import System.IO.Unsafe

-- Declare all testdata here

-- User ids:

user_eq, user_wechner, user_igel, user_spuall :: UserID
user_eq      = UserID "eq"      "alan"
user_wechner = UserID "wechner" "mroot"
user_igel    = UserID "igel"    "garten"
user_spuall  = UserID "spuall"  "spuall" -- XXX: Just a guess :)

-- ChannelIDs

channel_sep07, channel_barracuda, channel_csstuds :: ChannelID
channel_sep07     = ChannelID "sep07"     "sse.cs.tu-bs.de" -- :P
channel_barracuda = ChannelID "barracuda" "up.in.your.arse"
channel_csstuds   = ChannelID "csstuds"   "freenode.org"

-- ChannelNames

cname_sep07, cname_barracuda, cname_csstuds :: ChannelName
cname_sep07     = mkChannelName "SEP 07 Channel"
cname_barracuda = mkChannelName "Barracuda!"
cname_csstuds   = mkChannelName "CS-Studs"

-- Times

date_jfk,date_malcomX,date_mlk :: UTCTime
date_jfk     = UTCTime (fromGregorian 1963 11 22) (12*3600+30*60)
date_malcomX = UTCTime (fromGregorian 1965  2 21) 0
date_mlk     = UTCTime (fromGregorian 1968  4  4) (18*3600+ 1*60)

-- Addresses

addr_cia, addr_fsf, addr_penisland :: HostAddress
addr_cia       = unsafePerformIO $ inet_addr "198.81.129.100"
addr_fsf       = unsafePerformIO $ inet_addr "199.232.41.5"
addr_penisland = unsafePerformIO $ inet_addr "38.113.185.223"

-- Socket Adresses

saddr_cia, saddr_fsf, saddr_penisland :: SockAddr
saddr_cia       = SockAddrInet 8888 addr_cia
saddr_fsf       = SockAddrInet 8888 addr_fsf
saddr_penisland = SockAddrInet 7777 addr_penisland

-- Messages

routed_innocent, routed_nottl, routed_nack :: Routed TargetContent ExternalSignature
routed_innocent = Routed 14 user_wechner "29"
	(Message [user_igel,user_spuall] cname_sep07 channel_sep07
		(UnencryptedMessage "what was the price on his head?" [])
		date_malcomX
		0)
	Nothing
routed_nottl    = Routed 0 user_spuall "1788" (GetCertificate user_eq) Nothing
routed_nack     = Routed 5 user_igel   "80"
	(Nack (Routed 0 user_wechner "12" (Certificate [user_eq] user_spuall (pack [1,2,3])) Nothing))
	Nothing

-- Certificates

loadCert :: String -> IO Certificate
loadCert file = do
	str <- BS.readFile $ ".." </> "tests" </> "certificates" </> file
	case importCertificate str X509FmtPem of
		Right cert -> return cert
		Left err   -> let msg = "Failed to load certificate" ++ file ++ ": " ++ show err
			in putStrLn msg >> fail msg

loadKey :: String -> IO PrivateKey
loadKey file = do
	str <- BS.readFile $ ".." </> "tests" </> "certificates" </> file
	case importPrivateKey str X509FmtPem of
		Right key -> return key
		Left err  -> let msg = "Failed to load private key" ++ file ++ ": " ++ show err
			in putStrLn msg >> fail msg

dummyCerts :: IO [Certificate]
dummyCerts = mapM loadCert (map (\n -> "Dummy"++show n++"-cert.pem") [1..10])

unsafeDummyCerts :: [Certificate]
unsafeDummyCerts = unsafePerformIO dummyCerts

dummyKeys :: IO [PrivateKey]
dummyKeys = mapM loadKey (map (\n -> "Dummy"++show n++"-key.pem") [1..10])

unsafeDummyKeys :: [PrivateKey]
unsafeDummyKeys = unsafePerformIO dummyKeys
