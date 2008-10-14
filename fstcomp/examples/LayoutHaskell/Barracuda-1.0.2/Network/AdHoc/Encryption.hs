{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Maintainer: Henning Guenther
module Network.AdHoc.Encryption
	(Encrypted(..)
	,Encryptable(..)
	,RSAEncrypted(..)
	,RSAEncryptable(..)
	,pack64
	,unpack64
	,generateDESKey
	) where

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC
import Data.List (foldl',mapAccumL)
import Data.Word
import System.Random
import Codec.Utils (toOctets,fromOctets)
import qualified Codec.Encryption as Encryption (encrypt,decrypt)
import Codec.Encryption hiding (encrypt,decrypt)
import qualified Codec.Encryption.PKCS1 as PKCS1
import Network.GnuTLS.X509
import System.Random

-- | Represents encrypted data.
data Encrypted a = Encrypted
	{encryptedIV   :: Word64
	,encryptedData :: BS.ByteString
	} deriving (Show,Eq)

-- | Represents data encrypted by RSA.
data RSAEncrypted a = RSAEncrypted
	{rsaData :: BS.ByteString
	} deriving (Show,Eq)

-- | A class of types that can be encrypted with 64 bit keys and initial vectors.
class Encryptable a where
	-- | Encrypts an object into an 'Encrypted' object
	encrypt :: Word64	-- ^ The key to use
		-> Word64	-- ^ The initial vector
		-> a		-- ^ The object to be encrypted
		-> Encrypted a
	-- | Reverses the encrypt operation
	decrypt :: Word64	-- ^ The key to use
		-> Encrypted a	-- ^ The encrypted content to be decrypted
		-> Maybe a

-- | A class of types that can be encrypted with RSA.
class RSAEncryptable a where
	rsaEncrypt :: RandomGen g => g	-- ^ Random generator to generate padding
		   -> Certificate       -- ^ The 'Certificate' to use for encryption
		   -> a                 -- ^ Data to be encrypted
		   -> RSAEncrypted a
	rsaDecrypt :: PrivateKey  -> RSAEncrypted a -> Maybe a

instance Encryptable BS.ByteString where
	encrypt key iv str = Encrypted iv
		(Encryption.encrypt CipherDES ModeCBC key iv (pkcs5 str))
	decrypt key (Encrypted iv dat) = Encryption.decrypt CipherDES ModeCBC key iv dat >>= unpkcs5

instance Encryptable String where
	encrypt key iv str = coerceEncrypted $ encrypt key iv (BSC.pack str)
	decrypt key dat    = decrypt key (coerceEncrypted dat) >>= return.(BSC.unpack)

coerceEncrypted :: Encrypted a -> Encrypted b
coerceEncrypted (Encrypted iv dat) = Encrypted iv dat

-- | Packs a stream of 8-bit Words into a stream of 64-bit Words.
pack64 :: [Word8] -> [Word64]
pack64 []  = []
pack64 lst = let
	(now,later) = splitAt 8 lst
	val = fromOctets 256 now
	in val:pack64 later

-- | Converts a list of 64-bit words into a list of 8-bit words.
unpack64 :: [Word64] -> [Word8]
unpack64 = concatMap (\x -> map (fromIntegral.(shiftR x)) [56,48..0])

-- | Randomly creates a key for DES.
generateDESKey :: RandomGen g => g -> (Word64,g)
generateDESKey gen = foldl' (\(v,g) x -> let
	(cv,ng) = genParityWord8 g
	in ((shiftL v 8) .|. (fromIntegral cv),ng)) (0,gen) [0..7]

genParityWord8 :: RandomGen g => g -> (Word8,g)
genParityWord8 gen = let
	(v,ngen) = randomR (0,255) gen
	in (mkWord8Parity $ fromIntegral v,ngen)

checkParity :: Bits a => a -> Bool
checkParity wrd = testBit (sum [shiftR wrd x | x <- [0..((bitSize wrd)-1)]]) 0

mkWord8Parity :: Word8 -> Word8
mkWord8Parity wrd = let
	res = sum [ shiftR wrd x | x <- [0..6]]
	in if testBit res 0
		then clearBit wrd 7
		else setBit wrd 7

instance RSAEncryptable String where
	rsaEncrypt gen cert str = coerceRSAEncrypted $ rsaEncrypt gen cert (BSC.pack str)
	rsaDecrypt key  dat = fmap BSC.unpack $ rsaDecrypt key (coerceRSAEncrypted dat)

instance RSAEncryptable BS.ByteString where
	rsaEncrypt gen cert str = RSAEncrypted $ standardRSAEncrypt gen cert str
	rsaDecrypt key (RSAEncrypted dat) = standardRSADecrypt key dat

instance RSAEncryptable Word64 where
	rsaEncrypt gen cert wrd = coerceRSAEncrypted $ rsaEncrypt gen cert (PKCS1.i2osp 8 wrd)
	rsaDecrypt key enc = fmap PKCS1.os2ip $ rsaDecrypt key (coerceRSAEncrypted enc)

coerceRSAEncrypted :: RSAEncrypted a -> RSAEncrypted b
coerceRSAEncrypted (RSAEncrypted dat) = RSAEncrypted dat

splitBlock :: Int -> [Word8] -> [[Word8]]
splitBlock size [] = []
splitBlock size wrd = let (x,xs) = splitAt size wrd in x:(splitBlock size xs)

standardRSAEncrypt :: RandomGen g => g -> Certificate -> BS.ByteString -> BS.ByteString
standardRSAEncrypt gen cert str = case certificateRSAParameters cert of
	Left err -> error ("Internal GnuTLS error: "++show err)
	Right (n,e) -> let
		pkey = PKCS1.PublicKey (PKCS1.os2ip n) (PKCS1.os2ip e)
		in PKCS1.encrypt (BS.length n) pkey gen str

standardRSADecrypt :: PrivateKey -> BS.ByteString -> Maybe BS.ByteString
standardRSADecrypt key str = case privateKeyRSAParameters key of
	Left err -> error ("Internal GnuTLS error: "++show err)
	Right (n,e,d,p,q,u) -> let
		skey = PKCS1.PrivateKey $ Right $ PKCS1.PrivateKeyComplex 
			(PKCS1.os2ip n) (PKCS1.os2ip d) (PKCS1.os2ip p) (PKCS1.os2ip q) (PKCS1.os2ip u)
		in PKCS1.decrypt (BS.length n) skey str

{-
rsaPkcs1Encrypt :: RandomGen g => g -> ([Word8],[Word8]) -> [Word8] -> [Word8]
rsaPkcs1Encrypt gen (m,e) str
	| length str > length m - 11 = error "message too long"
	| otherwise = let
		ps = take (length m - length str - 3) (map fromIntegral $ randomRs (1,255) gen)
		em = 0x00:0x02:(ps++[0x00]++str)
		in RSA.encrypt (m,e) em

rsaPkcs1Decrypt :: ([Word8],[Word8]) -> [Word8] -> [Word8]
rsaPkcs1Decrypt (m,d) str = let
	em = RSA.decrypt (m,d) str
	in case em of
		(0x00:0x02:rest) -> let
			(ps,m) = break (==0) rest
			in if length ps < 8
				then []
				else drop 1 m
		_ -> []-}
