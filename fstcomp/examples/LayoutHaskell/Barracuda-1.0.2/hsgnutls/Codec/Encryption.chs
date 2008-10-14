
module Codec.Encryption
	(Cipher(..)
	,Word128(..)
	,Word192(..)
	,pkcs5
	,unpkcs5
	,module Codec.Encryption.Ciphers
	) where

#include <gcrypt.h>

import Foreign
import Foreign.C.Types
import Prelude hiding (catch)
import Control.Exception (catch)
import Data.Bits
import Data.ByteString(ByteString,useAsCStringLen)
import qualified Data.ByteString as BS
import Data.ByteString.Base(create)
import Data.List(foldl')
import System.IO.Unsafe(unsafePerformIO)
import Codec.Encryption.Ciphers

data Word128 = Word128 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 deriving (Show,Eq)
data Word192 = Word192 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 deriving (Show,Eq)

instance Num Word128 where
	fromInteger num = let
		v1 = fromInteger (shiftR num 64)
		v2 = fromInteger num
		in Word128 v1 v2

instance Num Word192 where
	fromInteger num = let
		v1 = fromInteger (shiftR num 128)
		v2 = fromInteger (shiftR num  64)
		v3 = fromInteger num
		in Word192 v1 v2 v3

class (Show ciph,Show mode,CipherType ciph,CipherMode mode,CipherDatum key,CipherDatum iv) =>
	Cipher ciph mode key iv | ciph mode -> key iv where
	encrypt :: ciph -> mode -> key -> iv -> ByteString -> ByteString
	encrypt ct cm ck ci str = unsafePerformIO $ do
		ciph <- cipherOpen ct cm []
		setKey ciph ck
		setIV ciph ci
		cipherEncrypt ciph str
	decrypt :: ciph -> mode -> key -> iv -> ByteString -> Maybe ByteString
	decrypt ct cm ck ci str = unsafePerformIO ((do
		ciph <- cipherOpen ct cm []
		setKey ciph ck
		setIV ciph ci
		cipherDecrypt ciph str >>= return.Just) `catch` (\_ -> return Nothing))

class CipherDatum k where
	setKey :: CipherHandle -> k -> IO ()
	setIV  :: CipherHandle -> k -> IO ()

instance CipherDatum Word64 where
	setKey = cipherSetKey
	setIV = cipherSetIV

instance CipherDatum () where
	setKey _ _ = return ()
	setIV  _ _ = return ()

instance CipherDatum Word128 where
	setKey = cipherSetKey 
	setIV = cipherSetIV

instance CipherDatum Word192 where
	setKey = cipherSetKey 
	setIV = cipherSetIV

instance Storable Word192 where
	sizeOf _ = 24
	alignment _ = 4
	peek ptr = do
		v1 <- peek (castPtr ptr)
		v2 <- peek (castPtr (plusPtr ptr 8))
		v3 <- peek (castPtr (plusPtr ptr 16))
		return (Word192 v1 v2 v3)
	poke ptr (Word192 v1 v2 v3) = do
		poke (castPtr ptr) v1
		poke (castPtr (plusPtr ptr 8)) v2
		poke (castPtr (plusPtr ptr 16)) v3

instance Storable Word128 where
	sizeOf _ = 16
	alignment _ = 4
	peek ptr = do
		v1 <- peek (castPtr ptr)
		v2 <- peek (castPtr (plusPtr ptr 8))
		return (Word128 v1 v2)
	poke ptr (Word128 v1 v2) = do
		poke (castPtr ptr) v1
		poke (castPtr (plusPtr ptr 8)) v2

instance Cipher CipherDES ModeECB Word64 ()
instance Cipher CipherDES ModeCBC Word64 Word64
instance Cipher Cipher3DES ModeECB Word192 ()
instance Cipher Cipher3DES ModeCBC Word192 Word192
instance Cipher CipherCast5 ModeECB Word128 ()
instance Cipher CipherBlowfish ModeECB Word128 ()
-- ...

{#pointer gcry_cipher_hd_t as CipherHandle foreign newtype#}

flags :: [CUInt] -> CUInt
flags = foldl' (.|.) 0

foreign import ccall unsafe "gcrypt.h &gcry_cipher_close" gcry_cipher_close :: FunPtr (Ptr CipherHandle -> IO ())

cipherOpen :: (Show tp,Show md,CipherType tp,CipherMode md) =>
	tp -> md -> [CipherFlag] -> IO CipherHandle
cipherOpen ct cm flg = alloca $ \hand -> do
	res <- {#call unsafe gcry_cipher_open#} hand (cipherTypeToC ct) (cipherModeToC cm) (flags (map flagToC flg))
	if res /= 0
		then error ("Failed to open cipher "++show ct++" with mode "++show cm++" and flags "++show flg)
		else (do
			fp <- newForeignPtr gcry_cipher_close =<< peek hand
			return $ CipherHandle fp)

#c
gcry_error_t gcry_cipher_setkey2 (gcry_cipher_hd_t H, void *K, size_t L);
gcry_error_t gcry_cipher_setiv2 (gcry_cipher_hd_t H, void *K, size_t L);
#endc

cipherSetKey :: Storable a => CipherHandle -> a -> IO ()
cipherSetKey hand dat = with dat $ \ptr -> cipherSetKey' hand (castPtr ptr) (sizeOf dat)

cipherSetKey' :: CipherHandle -> Ptr () -> Int -> IO ()
cipherSetKey' hand addr len = withCipherHandle hand $ \ptr -> do
		res <- {#call unsafe gcry_cipher_setkey2#} ptr addr (fromIntegral len)
		if res /= 0
			then error "Failed to set key"
			else return ()

cipherSetIV :: Storable a => CipherHandle -> a -> IO ()
cipherSetIV hand dat = with dat $ \ptr -> cipherSetIV' hand (castPtr ptr) (sizeOf dat)

cipherSetIV' :: CipherHandle -> Ptr () -> Int -> IO ()
cipherSetIV' hand addr len = withCipherHandle hand $ \ptr -> do
	res <- {#call unsafe gcry_cipher_setiv2#} ptr addr (fromIntegral len)
	if res /= 0
		then error "Failed to set IV"
		else return ()

cipherEncrypt :: CipherHandle -> ByteString -> IO ByteString
cipherEncrypt hand str = withCipherHandle hand $ \ptr ->
	useAsCStringLen str $ \(strp,len) ->
	create len $ \optr -> do
		res <- {#call unsafe gcry_cipher_encrypt#} ptr (castPtr optr) (fromIntegral len) (castPtr strp) (fromIntegral len)
		if res /= 0
			then error "Failed to encrypt data"
			else return ()

cipherDecrypt :: CipherHandle -> ByteString -> IO ByteString
cipherDecrypt hand str = withCipherHandle hand $ \ptr ->
	useAsCStringLen str $ \(strp,len) ->
	create len $ \optr -> do
		res <- {#call unsafe gcry_cipher_decrypt#} ptr (castPtr optr) (fromIntegral len) (castPtr strp) (fromIntegral len)
		if res /= 0
			then error "Failed to decrypt data"
			else return ()

pkcs5 :: ByteString -> ByteString
pkcs5 bs = let
	len = (BS.length bs) `mod` 8
	pad = BS.replicate (8-len) (fromIntegral (8-len))
	in bs `BS.append` pad

unpkcs5 :: ByteString -> Maybe ByteString
unpkcs5 bs = let
	len = BS.length bs
	pchar = BS.last bs
	(str,pad) = BS.splitAt (len-fromIntegral pchar) bs
	in if BS.all (==pchar) pad
		then Just str
		else Nothing
