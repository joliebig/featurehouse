module Tests.QuickCheck.X509(rsaParametersMatch,rsaVerify,rsaEncrypt) where

import qualified Codec.Encryption.PKCS1 as PKCS1
import Data.ByteString as BS hiding (map,zip)
import Test.QuickCheck
import Network.GnuTLS
import Tests.Data
import System.Random

instance Arbitrary Certificate where
	arbitrary = elements unsafeDummyCerts
	coarbitrary _ = id

instance Arbitrary PrivateKey where
	arbitrary = elements unsafeDummyKeys
	coarbitrary _ = id

instance Arbitrary DigestAlgorithm where
	arbitrary = elements [DigMd5,DigSha1,DigRmd160,DigMd2]
	coarbitrary _ = id

newtype KeyPair = KeyPair (PrivateKey,Certificate) deriving Show

instance Arbitrary KeyPair where
	arbitrary = elements (map KeyPair $ zip unsafeDummyKeys unsafeDummyCerts)
	coarbitrary _ = id

classifyPair :: Certificate -> PrivateKey -> Bool -> Bool -> Property
classifyPair cert key valY valN = let
	certId = certificateGetKeyId cert
	keyId  = privateKeyGetKeyId  key
	in classify (keyId == certId) "matching pairs"
		$ classify (keyId /= certId) "non-matching pairs"
		$ if keyId == certId then valY else valN

rsaParametersMatch :: Certificate -> PrivateKey -> Property
rsaParametersMatch cert key = let
	certPar = certificateRSAParameters cert
	keyPar  = do
		(n,e,d,p,q,u) <- privateKeyRSAParameters key
		return (n,e)
	in classifyPair cert key (certPar == keyPar) (certPar /= keyPar)

rsaVerify :: Certificate -> PrivateKey -> ByteString -> DigestAlgorithm -> Property
rsaVerify cert key str dig = let
	res = do
		sig <- signData key dig str
		verifySignature cert str sig
	in classifyPair cert key (res==Right True) (res==Right False)

instance Arbitrary StdGen where
	arbitrary = rand
	coarbitrary _ = id

rsaEncrypt :: StdGen -> Certificate -> PrivateKey -> ByteString -> Property
rsaEncrypt gen cert key str = let
	res = do
		(n,e) <- certificateRSAParameters cert
		(n',e',d,p,q,u) <- privateKeyRSAParameters key
		let pkey = PKCS1.PublicKey (PKCS1.os2ip n) (PKCS1.os2ip e)
		let skey = PKCS1.PrivateKey $ Right $ PKCS1.PrivateKeyComplex
			(PKCS1.os2ip n') (PKCS1.os2ip d) (PKCS1.os2ip p) (PKCS1.os2ip q) (PKCS1.os2ip u)
		let enc = PKCS1.encrypt (BS.length n) pkey gen str
		let dec = PKCS1.decrypt (BS.length n') skey enc
		return dec
	in (not $ BS.null str) ==> classifyPair cert key (res == Right (Just str)) (res == (Right Nothing))
