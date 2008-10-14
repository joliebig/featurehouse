{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Maintainer: Henning Guenther
--
-- This module is a helper for signature checking. It gives the text
-- that needs to be hashed in order to create or verify a signature.
module Network.AdHoc.Signature
	(Signature(..)
	,SignType(..)
	,SignatureStatus(..)
	,NoSignature
	,InternalSignature
	,ExternalSignature
	,ToInternalSignature(..)
	,getHashString
	,verifySignature
	) where

import Data.ByteString hiding (foldl,concatMap)
import Text.XML.HaXml.Types
import Data.Word
import Data.Char.UTF8
import qualified Network.GnuTLS.X509 as X509

-- | A raw signature.
data Signature = Signature
	{sign_type :: SignType   -- ^ The type of the signature
	,signature :: ByteString -- ^ The actual signature
	}
	deriving (Show,Eq)

-- | Specifies the signature algorithm used to sign a message.
data SignType
	= MD5 -- ^ MD5 has been used to sign the message
	| SignUnknown String -- ^ An unknown signature algorithm was used
	deriving (Show,Eq)

-- | This type represents the possible results of verifying a signature.
data SignatureStatus
	= SignatureOK    -- ^ The signature was valid
	| SignatureWrong -- ^ The signature was invalid
	| CertificateMissing (X509.Certificate -> SignatureStatus) -- ^ The signature could not be
		-- checked, becuase a 'X509.Certificate' was missing. The function
		-- is a callback to be called when the missing certificate has been
		-- acquired.

instance Show SignatureStatus where
	show SignatureOK = "SignatureOK"
	show SignatureWrong = "SignatureWrong"
	show (CertificateMissing cb) = "CertificateMissing"

instance Eq SignatureStatus where
	SignatureOK == SignatureOK		= True
	SignatureWrong == SignatureWrong	= True
	_ == _ 					= False

-- | No signature has been given.
type NoSignature = ()

-- | The message has been locally created, so we know the private key.
type InternalSignature = Either X509.PrivateKey ExternalSignature

-- | The signature has been received via network. The signature might be
--   knwon or not.
type ExternalSignature = Maybe (Signature,SignatureStatus)

-- | A class of types that have an internal signature status.
class ToInternalSignature s where
	toInternal :: s -> InternalSignature

instance ToInternalSignature NoSignature where
	toInternal _ = Right Nothing

instance ToInternalSignature ExternalSignature where
	toInternal = Right

instance ToInternalSignature InternalSignature where
	toInternal = id

-- | Given a certificate, this function verifies the Signature of a 'String'.
verifySignature :: String -> Signature -> X509.Certificate -> SignatureStatus
verifySignature str sig cert = case X509.verifySignature cert (toUTF8 str) (signature sig) of
	Left err -> CertificateMissing (verifySignature (toUTF8 str) sig)
	Right False -> SignatureWrong
	Right True -> SignatureOK

-- | Calculates the concatenation of all text nodes in the list of contents, except for receiver elements
getHashString :: [Content i] -> String
getHashString = concatMap getHashStringContent

getHashStringContent :: Content i -> String
getHashStringContent (CElem (Elem "receiver" _ _) _) = "" -- As given by standard
getHashStringContent (CElem (Elem _ _ conts) _) = getHashString conts
getHashStringContent (CString _ str _) = str
getHashStringContent _ = ""

getHashStringDocument :: Document i -> String
getHashStringDocument (Document _ _ (Elem _ _ conts) _) = getHashString conts
