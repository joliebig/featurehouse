-- |
-- Maintainer: Henning Guenther
--
-- This module contains types representing the different kinds of
-- messages defined by the protocol.
module Network.AdHoc.Message where

import Data.ByteString.Lazy as LBS
import Data.ByteString as BS
import Data.Word
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Maybe
import Network.GnuTLS.X509 (Certificate,PrivateKey)

import Network.AdHoc.Channel
import Network.AdHoc.Encryption
import Network.AdHoc.MessageID
import Network.AdHoc.UserID
import Network.AdHoc.Signature

-- | Specifies the encryption algorithm used to encrypt a message
data CipherType
	= CipherDES_CBC
	| CipherNone
	| CipherUnknown String -- ^ An unknown cipher was used
	deriving (Show,Eq)

-- | TimeToLive has a maximum of 360, so 16 bit unsigned is enough to hold it
type TTL = Word16

-- | The message-delay can be represented by this type.
type Delay = Word

-- | An attachmet as defined in the protocoll.
data Attachment = Attachment
	{attachmentFilename :: String       -- ^ Name of the attached file
	,attachmentAppType :: String        -- ^ Mime-type of the very same
	,attachmentContent :: BS.ByteString -- ^ The actual content of the file
	} deriving (Show,Eq)

-- | An attachment that has been encrypted.
data EncryptedAttachment = EncryptedAttachment
	{encryptedAttachmentFilename :: Encrypted String        -- ^ Encrypted filename
	,encryptedAttachmentAppType  :: Encrypted String        -- ^ Encrypted mime-type
	,encryptedAttachmentContent  :: Encrypted BS.ByteString -- ^ Encrypted content
	} deriving (Show,Eq)

-- | Uses DES to encrypt an 'Attachment'.
encryptAttachment :: Word64 -- ^ Key
	-> Word64           -- ^ IV
	-> Attachment       -- ^ 'Attachment' to be encrypted
	-> EncryptedAttachment
encryptAttachment key iv (Attachment name tp cont) = EncryptedAttachment
	(encrypt key iv name)
	(encrypt key iv tp)
	(encrypt key iv cont)

-- | Uses DES to decrypt an 'EncryptedAttachment'.
decryptAttachment :: Word64    -- ^ Key
	-> EncryptedAttachment -- ^ Encrypted attachment
	-> Maybe Attachment    -- ^ 'Nothing' at failure, 'Just' when successful
decryptAttachment key (EncryptedAttachment name tp cont) = do
	rname <- decrypt key name
	rtp   <- decrypt key tp
	rcont <- decrypt key cont
	return $ Attachment rname rtp rcont

-- | A class of messages that can be routed.
data Routed a sign = Routed
	{routedTTL :: TTL         -- ^ The time-to-live of the message
	,routedUserID :: UserID   -- ^ Receiver of the routed message
	,routedMsgID :: MessageID -- ^ ID of the routed message
	,routedContent :: a       -- ^ The actual content of the message
	,routedSignature :: sign  -- ^ Signature of the message
	} deriving (Show,Eq)

-- | An unsigned message.
type UnsignedMessage = ProtocolMessage NoSignature

-- | An internal message that is subject to internal signature guidelines.
type InternalMessage = ProtocolMessage InternalSignature

-- | An external message.
type ExternalMessage = ProtocolMessage ExternalSignature

-- | Basic protocol message representation.
data ProtocolMessage sign
	= Hello 
		{helloSenders :: [UserID]
		,helloVersion :: Int
		,helloGreeting :: Maybe String
		} -- ^ Informs about a user on the sending node, giving the user-id, the protocol version and a friendly greeting
	| Ack
		{ackSender	:: UserID
		,ackMsgId	:: MessageID
		} -- ^ Sender of the original message and the sended message-id
	| Routing
		{routingRoutes :: [(UserID, Int)]
		}-- ^ Routing informations, a list of users and how much hops it takes to reach them
	| Target (Routed TargetContent sign) -- ^ Messages with one specified receiver
	| Flood	(Routed FloodContent sign)   -- ^ Messages flooded through the network
	| Obscure (Routed (RSAEncrypted String) ()) -- ^ Obscure messages
	deriving (Show,Eq)

-- | Messages directed to a specified receiver.
data TargetContent
	= Nack (Routed TargetContent ExternalSignature) -- ^ Negative ACK
	| GetCertificate
		{getCertificateFor :: UserID
		} -- ^ Requesting a certificate
	| Certificate
		{certificateReceivers :: [UserID]
		,certificateFor :: UserID
		,certificateData :: BS.ByteString
		} -- ^ Transmitting a certificate
	| Message
		{messageReceivers :: [UserID]
		,messageChannelName :: ChannelName
		,messageChannelID :: ChannelID
		,messageContent :: MessageContent
		,messageTime :: UTCTime
		,messageDelay :: Delay
		} -- ^ A chat message
	| GetKey
		{getKeyReceiver :: UserID
		,getKeyChannelName :: ChannelName
		,getKeyChannelID :: ChannelID
		} -- ^ Key request for a private channel
	| Key
		{keyReceiver :: UserID
		,keyChannelName :: ChannelName
		,keyChannelID :: ChannelID
		,keyCipherType :: CipherType
		,keyKey :: RSAEncrypted Word64
		} -- ^ Key message for a private channel
	deriving (Show,Eq)

-- | Messages flooded throughout the entire network.
data FloodContent
	= Channel
		{channelChannelName :: ChannelName
		,channelChannelID :: ChannelID
		,channelChannelTitle :: String
		,channelUsers :: [UserID]
		,channelPrivate :: Bool
		} -- ^ Channel announcements
	| Join
		{joinChannelName :: ChannelName
		,joinChannelID :: ChannelID
		} -- ^ Join message for a channel
	| Leave
		{leaveChannelName :: ChannelName
		,leaveChannelID :: ChannelID
		} -- ^ The users leaves a channel
	| Anonymous
		{anonymousText :: String
		,anonymousAttachments :: [Attachment]
		,anonymousTime :: UTCTime
		,anonymousDelay :: Delay
		} -- ^ An anonymous message that has been unpacked and will be flooded
	deriving (Show,Eq)

-- | A messages content can either be encrypted or not.
data MessageContent
	= EncryptedMessage (Encrypted String) [EncryptedAttachment]
	| UnencryptedMessage String [Attachment]
	deriving (Show,Eq)

-- | Decrements the time-to-live of a message. If it sinks under zero,
--   'Nothing' is returned. 'Just' @msg@ otherwise, where @msg@ has a
--   decremented ttl.
decrementTTL :: Routed a sign -> Maybe (Routed a sign)
decrementTTL (Routed ttl user mid cont sig)
	| ttl == 0	= Nothing
	| otherwise	= Just (Routed (ttl-1) user mid cont sig)

-- | Extracts the information from a message, that indicates, where to
--   route it.
routeTo :: Routed TargetContent sign -> [UserID]
routeTo (Routed _ _ _ (Nack (Routed _ from _ _ _)) _) = [from]
routeTo (Routed _ _ _ (GetCertificate for) _) = [for]
routeTo (Routed _ _ _ (Certificate receivers _ _) _) = receivers
routeTo (Routed _ _ _ (Message receivers _ _ _ _ _) _) = receivers
routeTo (Routed _ _ _ (GetKey receiver _ _) _) = [receiver]
routeTo (Routed _ _ _ (Key receiver _ _ _ _) _) = [receiver]

instance Functor (Routed a) where
	fmap f (Routed ttl user msgid cont sig) = Routed ttl user msgid cont (f sig)

instance Functor ProtocolMessage where
	fmap _ (Hello recv vers greet) = Hello recv vers greet
	fmap _ (Ack send msgid) = Ack send msgid
	fmap _ (Routing rt) = Routing rt
	fmap f (Target rt) = Target (fmap f rt)
	fmap f (Flood x) = Flood (fmap f x)
	fmap _ (Obscure rt) = Obscure rt
