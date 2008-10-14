module Tests.QuickCheck.Parser
	(testGeneratorParserId) where

import Control.Monad
import Test.QuickCheck hiding (generate)

import Network.AdHoc.ParserStrict
import Network.AdHoc.Encryption
import Network.AdHoc.Generator (generateMessage)
import Network.AdHoc.Message
import Network.AdHoc.UserID
import Network.AdHoc.Channel
import Network.AdHoc.Signature

import Text.XML.HaXml.Pretty(document)
import Text.XML.HaXml.Parse(xmlParse)

import Data.ByteString (pack)
import Data.Word
import System.Random
import Data.Time.Clock
import Data.Time.Calendar

genAscii :: Gen Char
genAscii = elements (['a'..'z']++['A'..'Z'])

genAsciiString :: Int -> Gen String
genAsciiString min = sized (\len -> replicateM (len+min) genAscii)

genUser :: Gen UserID
genUser = do
	id <- genAsciiString 1
	host <- genAsciiString 1
	return $ UserID id host

genChannelID :: Gen ChannelID
genChannelID = do
	cvalue <- genAsciiString 1
	chost <- genAsciiString 1
	return $ ChannelID cvalue chost

genList :: Int -> Gen a -> Gen [a]
genList min gen = sized $ \len -> replicateM (min+len) gen

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = oneof [return Nothing,gen>>=return.Just]

genWord8 :: Gen Word8
genWord8 = choose (0,255) >>= return.fromIntegral

genWord64 :: Gen Word64
genWord64 = choose (0,18446744073709551615) >>= return.fromIntegral

msgGen :: Gen InternalMessage
msgGen = frequency
	[(1,genHello)
	,(1,genAck)
	,(1,genRouting)
	,(6,genTarget)
	,(4,genFlood)
	,(1,genObscure)]

genHello :: Gen InternalMessage
genHello = do
	users <- genList 1 genUser
	vers <- choose (1,100)
	greet <- genMaybe (genAsciiString 0)
	return $ Hello users vers greet

genAck :: Gen InternalMessage
genAck = do
	sender <- genUser
	msgid <- genAsciiString 1
	return $ Ack sender msgid

genRouting :: Gen InternalMessage
genRouting = do
	routes <- sized $ \len -> replicateM len (do
		user <- genUser
		hops <- choose (1,30)
		return (user,hops)
		)
	return $ Routing routes

genTarget :: Gen InternalMessage
genTarget = do
	rt <- fmap (fmap toInternal) $ genRoutedTarget False
	return (Target rt)

genRouted :: Gen a -> Gen (Routed a ExternalSignature)
genRouted inner = do
	ttl <- choose (0,360) >>= return.fromIntegral
	user <- genUser
	msgid <- genAsciiString 1
	cont <- inner
	return $ Routed ttl user msgid cont Nothing

genRoutedTarget :: Bool -> Gen (Routed TargetContent ExternalSignature)
genRoutedTarget in_nack = do
	let withoutNack =
		[genGetCertificate
		,genCertificate
		,genMessage
		,genGetKey
		,genKey]
	genRouted (if in_nack
		then oneof withoutNack
		else oneof $ [genNack]++withoutNack)

genNack :: Gen TargetContent
genNack = do
	rt <- genRoutedTarget True
	return (Nack rt)

genGetCertificate :: Gen TargetContent
genGetCertificate = genUser >>= return.(GetCertificate)

genCertificate :: Gen TargetContent
genCertificate = do
	receivers <- genList 1 genUser
	for <- genUser
	dat <- genList 1 genWord8
	return $ Certificate receivers for (pack dat)

genAttachment :: Gen Attachment
genAttachment = do
	filename <- genAsciiString 1
	appType <- genAsciiString 1
	content <- fmap pack $ genList 1 genWord8
	return $ Attachment filename appType content

genTime :: Gen UTCTime
genTime = do
	day <- choose (20000,60000)
	secs <- choose (0,60*60*24)
	return $ UTCTime
		{utctDay = ModifiedJulianDay day
		,utctDayTime = fromInteger secs
		}

genMessage :: Gen TargetContent
genMessage = do
	receivers <- genList 1 genUser
	cname <- genAsciiString 1
	cid <- genChannelID
	content <- genMessageContent
	time <- genTime
	delay <- choose (0,360) >>= return.fromIntegral
	return $ Message receivers (mkChannelName cname) cid content time delay

genMessageContent :: Gen MessageContent
genMessageContent = do
	txt <- genAsciiString 1
	key <- genMaybe genWord64
	attachments <- genList 0 genAttachment
	case key of
		Just rkey -> do
			iv <- genWord64
			encattach <- mapM (\attach -> do
				aiv <- genWord64
				return $ encryptAttachment rkey aiv attach
				) attachments
			return $ EncryptedMessage (encrypt rkey iv txt) encattach
		Nothing -> return $ UnencryptedMessage txt attachments
genGetKey :: Gen TargetContent
genGetKey = do
	receiver <- genUser
	cname <- genAsciiString 1
	cid <- genChannelID
	return $ GetKey receiver (mkChannelName cname) cid

genCipher :: Gen CipherType
genCipher = oneof [return CipherDES_CBC
		  ,return CipherNone
		  ,genAsciiString 1 >>= return.CipherUnknown]

genKey :: Gen TargetContent
genKey = do
	receiver <- genUser
	cname <- genAsciiString 1
	cid <- genChannelID
	cipher <- genCipher
	key <- fmap RSAEncrypted $ fmap pack $ genList 1 genWord8
	return $ Key receiver (mkChannelName cname) cid cipher key

genFlood :: Gen InternalMessage
genFlood = do
	rt <- fmap (fmap toInternal) $ genRouted (oneof
		[genChannel
		,genJoinLeave True
		,genJoinLeave False
		,genAnonymous
		])
	return $ Flood rt

genChannel :: Gen FloodContent
genChannel = do
	cname <- genAsciiString 1
	cid <- genChannelID
	title <- genAsciiString 1
	users <- genList 1 genUser
	priv <- arbitrary
	return $ Channel (mkChannelName cname) cid title users priv

genJoinLeave :: Bool -> Gen FloodContent
genJoinLeave join = do
	cname <- genAsciiString 1
	cid <- genChannelID
	return $ (if join then Join else Leave) (mkChannelName cname) cid

genAnonymous :: Gen FloodContent
genAnonymous = do
	text <- genAsciiString 1
	attach <- genList 0 genAttachment
	time <- genTime
	delay <- choose (0,360) >>= return.fromIntegral
	return $ Anonymous text attach time delay

genObscure :: Gen InternalMessage
genObscure = fmap Obscure $ fmap (fmap $ const ()) $ genRouted (fmap RSAEncrypted $ fmap pack $ genList 0 genWord8)

classifyMessage :: ProtocolMessage sign -> String
classifyMessage (Hello _ _ _) = "Hello"
classifyMessage (Ack _ _) = "Ack"
classifyMessage (Routing _) = "Routing"
classifyMessage (Target (Routed _ _ _ tc _)) = case tc of
	Nack _ -> "Nack"
	GetCertificate _ -> "GetCertificate"
	Certificate _ _ _ -> "Certificate"
	Message _ _ _ _ _ _ -> "Message"
	GetKey _ _ _ -> "GetKey"
	Key _ _ _ _ _ -> "Key"
classifyMessage (Flood (Routed _ _ _ fc _)) = case fc of
	Channel _ _ _ _ _ -> "Channel"
	Join _ _ -> "Join"
	Leave _ _ -> "Leave"
	Anonymous _ _ _ _ -> "Anonymous"
classifyMessage (Obscure _) = "Obscure"

genParseId parser = forAll msgGen (\msg -> classify True (classifyMessage msg) $
	(parser (xmlParse "inp" (fst (generateMessage (map show [0..]) msg))))
	== Right (fmap (const Nothing) msg))

testGeneratorParserId = genParseId parseMessageNoValidate
