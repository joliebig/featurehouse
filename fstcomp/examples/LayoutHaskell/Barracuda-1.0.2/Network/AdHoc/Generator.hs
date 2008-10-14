{-# LANGUAGE FlexibleContexts #-}
-- |
-- Maintainer: Martin Wegner
-- Date: 2007-06-15

module Network.AdHoc.Generator
	( generateMessage
	, generateMessage'
	, genRootElem
	, genAnonymous
	, genObscure
	)
	where

import Codec.Binary.Base64

import Data.ByteString hiding (map,concat,head,tail,takeWhile)
import qualified Data.ByteString as BS
import Data.Char
import Data.Char.UTF8 (toUTF8)
import Data.Word
import Data.Time
import Network.GnuTLS
import Network.GnuTLS.X509
import System.Locale(defaultTimeLocale)
import Text.XML.HaXml.Escape
import Text.XML.HaXml.Parse hiding (document)
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Types
import Text.XML.HaXml.XmlContent

import Network.AdHoc.Channel
import Network.AdHoc.Encryption
import Network.AdHoc.Message
import Network.AdHoc.MessageID
import Network.AdHoc.Signature
import Network.AdHoc.UserID
import Network.AdHoc.XMLRenderer

-- | This structure represents an abstract element tree to be used for generating the XML afterwards.
--   It is designed for the special needs of the protocol schema and thus not as common as the HaXML
--   structures.
data AbstractElement = AbstractElement (String, [ (String, String) ], Either String [ AbstractElement ]) deriving (Show)

--
-- Time helper function
--

showTime :: UTCTime -> String
showTime = (takeWhile (/='.')).(formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S") -- XXX: The takeWhile is only required in older versions of the time package

--
-- Signature creation helper functions
--

genStringToSign :: AbstractElement -> String
genStringToSign (AbstractElement (name, _, children))
	= case children of
	      Left str -> if name == "receiver" then "" else str
	      Right elems -> concat $ map genStringToSign elems

genSignature :: AbstractElement -> PrivateKey -> ByteString
genSignature elem key
	= let signed = (signData key DigMd5 (toUTF8 $ genStringToSign elem)) in
		case signed of
		    Left err -> error $ "Internal GnuTLS error: " ++ show err
		    Right bs -> bs

--
-- Common helper functions for generating the tree
--

genXMLCont :: Element () -> Content()
genXMLCont elem = CElem elem ()

genXMLElem :: AbstractElement -> Element ()
genXMLElem (AbstractElement (name, attributes, children))
	= Elem name [ mkAttr (fst a) (snd a)	| a <- attributes ] content
		where
			content = case children of
			              Left str    -> [ CString False str () ]
			              Right elems -> genXMLElems elems

genXMLElems :: [ AbstractElement ] -> [ Content () ]
genXMLElems = map (genXMLCont.genXMLElem)

-- Helper functions for generating the internal abstract tree

-- | Takes the given element and puts it into the root element of a message.
genRootElem :: String -> AbstractElement -> Bool -> Maybe Delay -> TTL -> InternalSignature -> AbstractElement
genRootElem name elem flood delay ttl sign
	= AbstractElement (name, attributes, Right [ elem ])
		where
			delayAttr         = case delay of
			                        Just d  -> [ ("delay", (show d)) ]
			                        Nothing -> []
			defaultAttributes =
				[
					("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"),
					("xsi:schemaLocation", "http://www.ibr.cs.tu-bs.de/chat-message http://www.ibr.cs.tu-bs.de/courses/ss07/sep-cm/protocol/chat-message.xsd"),
					("xmlns", "http://www.ibr.cs.tu-bs.de/chat-message"),
					("flood", (map toLower $ show flood)),
					("ttl", (show ttl))
				]
			signature         = case sign of
			                        Left key                -> Just (Signature MD5 (genSignature elem key))
			                        Right (Just (rsign, _)) -> Just rsign
			                        _                       -> Nothing
			signAttr          = case signature of
			                        Just (Signature t s) -> [ ("signtype", show t), ("signature", encode $ unpack s) ]
			                        Nothing              -> []
			attributes        = delayAttr++defaultAttributes++signAttr

genRootElemDefault :: AbstractElement -> AbstractElement
genRootElemDefault elem = genRootElem "chat-message" elem False Nothing 360 (Right Nothing)

str :: String -> String -> AbstractElement
str n v = AbstractElement (n, [], Left v)

--
-- Generate internal abstract tree for the different nested messages
--

-- ACK
genAck :: UserID -> MessageID -> AbstractElement
genAck userID msgID = AbstractElement ("ack", [], Right [ str "sender" (show userID), str "messageid" msgID ])

-- HELLO
genHello :: MessageID -> [ UserID ] -> Int -> Maybe String -> AbstractElement
genHello msgid senders version greeting
	= AbstractElement ("hello", [], Right (senders'++[ str "messageid" msgid, str "version" (show version) ]++lGreeting))
		where
			senders'  = map ((str "sender").show) senders
			lGreeting = case greeting of
			                Just rgreeting -> [ str "greeting" rgreeting ]
			                Nothing        -> []

-- ROUTING
genRouting :: MessageID -> [ (UserID, Int) ] -> AbstractElement
genRouting msgid routes
	= AbstractElement ("routing", [], Right ([ str "messageid" msgid ]++destinations))
		where
			destinations = [ AbstractElement ("destination", [], Right [ str "user" (show user), str "hops" (show hops) ])	| (user, hops) <- routes ]

-- GETCERTIFICATE
genGetCertificate :: UserID -> MessageID -> UserID -> AbstractElement
genGetCertificate sender msgID for
	= AbstractElement ("getcertificate", [], Right [ str "sender" (show sender), str "receiver" (show for), str "messageid" msgID ])

-- CERTIFICATE
genCertificate :: UserID -> MessageID -> [ UserID ] -> UserID -> [ Word8 ] -> AbstractElement
genCertificate sender msgID receivers for certData
	= AbstractElement ("certificate", [], Right ([ sender' ]++receivers'++[ msgID' ]++[ certificate ]))
		where
			sender'     = str "sender" (show sender)
			receivers'  = map ((str "receiver").show) receivers
			msgID'      = str "messageid" msgID
			certificate = AbstractElement ("certificate", [], Right ([ str "user" (show for), str "data" (encode certData) ]))

-- GETKEY
genGetKey :: UserID -> MessageID -> UserID -> ChannelName -> ChannelID -> AbstractElement
genGetKey sender msgID receiver cname cid
	= AbstractElement ("getkey", [], Right elems)
		where
			elems = [
			          str "sender" (show sender), str "receiver" (show receiver), str "messageid" msgID,
			          str "channel" (show cname), str "channelid" (show cid)
			        ]

-- KEY
genKey :: UserID -> MessageID -> UserID -> ChannelName -> ChannelID -> CipherType -> ByteString -> AbstractElement
genKey sender msgID receiver cname cid cipher key
	= AbstractElement ("key", [], Right elems)
		where
			cipher' = case cipher of
			              CipherDES_CBC     -> "DES-CBC"
			              CipherNone        -> "NONE"
			              CipherUnknown str -> str
			elems   = [ 
			            str "sender" (show sender), str "receiver" (show receiver), str "messageid" msgID,
			            str "channel" (show cname), str "channelid" (show cid), str "cipher" cipher', str "key" (encode (unpack key))
			          ]

-- OBSCURE
-- | Generates the tree of 'AbstractElement's for an obscure message.
genObscure :: UserID -> Maybe MessageID -> ByteString -> AbstractElement
genObscure receiver msgID obscData
	= AbstractElement ("obscure", [], Right ([ str "receiver" (show receiver) ]++messageID++[ str "text" (encode $ unpack obscData) ]))
	where
		messageID = case msgID of
		                Just rmsgID -> [ str "messageid" rmsgID ]
		                Nothing     -> []

-- attachment
genAttachment :: Attachment -> AbstractElement
genAttachment (Attachment filename appType fileData)
	= AbstractElement ("attachment", [], Right children)
		where
		children = [
		             str "filename" filename, str "applicationtype" appType,
		             str "data" (encode $ BS.unpack fileData)
		           ]

genEncryptedAttachment :: EncryptedAttachment -> AbstractElement
genEncryptedAttachment (EncryptedAttachment filename appType fileData)
	= AbstractElement ("attachment", ivAttr, Right children)
		where
			ivAttr   = [ ("iv", encode (unpack64 [ encryptedIV filename ])) ]
			children = [
			             str "filename" (encode $ BS.unpack $ encryptedData filename),
			             str "applicationtype" (encode $ BS.unpack $ encryptedData appType),
			             str "data" (encode $ BS.unpack $ encryptedData fileData)
			           ]

-- MESSAGE text and attachment's
genMessageContent :: MessageContent -> (AbstractElement, [ AbstractElement ])
genMessageContent cont
	= case cont of
		UnencryptedMessage txt attach -> (str "text" txt,map genAttachment attach)
		EncryptedMessage   txt attach -> (
			AbstractElement ("text" ,[ ("iv", encode $ unpack64 [encryptedIV txt]) ],Left (encode $ BS.unpack $ encryptedData txt)),
			map genEncryptedAttachment attach
			)

-- MESSAGE
genMessage :: Maybe UserID -> Maybe MessageID -> [ UserID ] -> Maybe (ChannelName, ChannelID) -> MessageContent -> UTCTime -> AbstractElement
genMessage sender msgID receivers chan cont time
	= AbstractElement ("message", [], Right (sender'++receivers'++messageID++others++channel++[ elemText ]++attachments))
		where
			sender'      = case sender of
			                   Just rsender -> [ str "sender" (show rsender) ]
			                   Nothing      -> []
			receivers'   = map ((str "receiver").show) receivers
			messageID    = case msgID of
			                   Just rmsgID -> [ str "messageid" rmsgID ]
			                   Nothing     -> []
			others       = [ str "timestamp" (showTime time) ]
			channel      = case chan of
			                   Nothing -> []
			                   Just (cname, cid) -> [ str "channel" (show cname), str "channelid" (show cid) ]
			(elemText, attachments)
			             = genMessageContent cont

-- NACK
genNack :: UserID -> MessageID -> Routed TargetContent ExternalSignature -> AbstractElement
genNack sender msgID routed 
	= AbstractElement ("nack", [], Right (others++[ message ]))
		where
			-- Nested message is named "message" instead of "chat-message":
			message = generateElementFromRouted "message" routed
			others  = [ str "sender" (show sender), str "messageid" msgID ]

-- CHANNEL
genChannel :: UserID -> MessageID -> ChannelName -> ChannelID -> String -> [ UserID ] -> Bool -> AbstractElement
genChannel sender msgID cname cid title members private
	= AbstractElement ("channel", [ ("closed", map toLower (show private)) ], Right (others++members'))
		where
			others   = [
			             str "sender" (show sender), str "messageid" msgID,
			             str "channel" (show cname), str "channelid" (show cid), str "description" title
			           ]
			members' = map ((str "member").show) members

-- JOIN/LEAVE
genJoinLeave :: String -> UserID -> MessageID -> ChannelName -> ChannelID -> AbstractElement
genJoinLeave what who msgID cname cid
	= AbstractElement (what, [], Right [ str "sender" (show who), str "messageid" msgID, str "channel" (show cname), str "channelid" (show cid) ])

-- MESSAGE to channel "Anonymous"
-- | Generates the tree of 'AbstractElement's for an anonymous message.
genAnonymous :: Bool -> Maybe UserID -> Maybe MessageID -> String -> [ Attachment ] -> UTCTime -> AbstractElement
genAnonymous showChan sender msgID text attachments time
	= genMessage sender msgID [] channel (UnencryptedMessage text attachments) time
		where
			channel = if showChan
			              then Just (anonymous,ChannelID "Anonymous" "Anonymous")
			              else Nothing

generateElementFromRouted :: ToInternalSignature sig => String -> Routed TargetContent sig -> AbstractElement
generateElementFromRouted name (Routed ttl user messageID content sign)
	= genRootElem name elem False delay ttl (toInternal sign)
		where
			elem  = case content of
			            GetCertificate for
			                -> genGetCertificate user messageID for
			            Certificate receivers for certData
			                -> genCertificate user messageID receivers for (unpack certData)
			            GetKey receiver cname cid
			                -> genGetKey user messageID receiver cname cid
			            Key receiver cname cid cipher key
			                -> genKey user messageID receiver cname cid cipher (rsaData key)
			            Message receivers cname cid cont time _
			                -> genMessage (Just user) (Just messageID) receivers (Just (cname, cid)) cont time
			            Nack routed
			                -> genNack user messageID routed
			delay = case content of
			            Message _ _ _ _ _ rdelay -> Just rdelay
			            _                        -> Nothing

--
-- Generate internal abstract tree for messages represented by InternalMessage
--

generateElementWithMsgID :: InternalMessage -> MessageID -> AbstractElement
generateElementWithMsgID _ _ = undefined

generateElement :: [MessageID] -> InternalMessage -> (AbstractElement,[MessageID])
generateElement ids (Ack userID messageID) = (genRootElemDefault $ genAck userID messageID, ids)
generateElement ids (Hello receivers version greeting)
	= (genRootElemDefault $ genHello (head ids) receivers version greeting, tail ids) 
generateElement ids (Routing routes) = (genRootElemDefault $ genRouting (head ids) routes, tail ids)
generateElement ids (Target routed) = (generateElementFromRouted "chat-message" routed, ids)
generateElement ids (Flood (Routed ttl sender messageID content sign))
	= (genRootElem "chat-message" elem True delay ttl sign, ids)
		where
			elem  = case content of
			            Channel cname cid title members private -> genChannel sender messageID cname cid title members private 
			            Join cname cid                          -> genJoinLeave "join" sender messageID cname cid
			            Leave cname cid                         -> genJoinLeave "leave" sender messageID cname cid
			            Anonymous text attachments time _       -> genAnonymous True (Just sender) (Just messageID) text attachments time
			delay = case content of
			            Anonymous _ _ _ rdelay -> Just rdelay
			            _                      -> Nothing
generateElement ids (Obscure (Routed ttl receiver messageID content ()))
	= (genRootElem "chat-message" (genObscure receiver (Just messageID) (rsaData content)) False Nothing ttl (Right Nothing), ids)

--
-- Generate XML document tree for messages represented by InternalMessage
--

generateDocument :: Element () -> Document()
generateDocument elem
	= Document
		(Prolog (Just (XMLDecl "1.0" (Just (EncodingDecl "utf-8")) Nothing)) [] Nothing [])
		emptyST
		(xmlEscape escaper elem)
		[]

-- | Takes an 'AbstractElement' and generates the XML document from it.
generateMessage' :: AbstractElement -> String
generateMessage' = toUTF8.renderDocument.generateDocument.genXMLElem

-- | Takes an 'InternalMessage' and generates the XML document from it.
generateMessage :: [MessageID] -> InternalMessage -> (String,[MessageID])
generateMessage ids msg = let (el,nids) = (generateElement ids msg) in (generateMessage' el,nids)

