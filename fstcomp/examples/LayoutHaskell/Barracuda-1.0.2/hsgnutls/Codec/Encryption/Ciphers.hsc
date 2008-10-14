module Codec.Encryption.Ciphers where

#include <gcrypt.h>

import Foreign.C.Types

class CipherType a where
	cipherTypeToC :: a -> CInt

class CipherMode a where
	cipherModeToC :: a -> CInt

data CipherIDEA = CipherIDEA deriving Show
data Cipher3DES = Cipher3DES deriving Show
data CipherCast5 = CipherCast5 deriving Show
data CipherBlowfish = CipherBlowfish deriving Show
data CipherAES = CipherAES deriving Show
data CipherAES128 = CipherAES128 deriving Show
data CipherAES192 = CipherAES192 deriving Show
data CipherAES256 = CipherAES256 deriving Show
data CipherRIJNDAEL = CipherRIJDNDAEL deriving Show
data CipherRIJNDAEL128 = CipherRIJNDAEL128 deriving Show
data CipherRIJNDAEL192 = CipherRIJNDAEL192 deriving Show
data CipherRIJNDAEL256 = CipherRIJNDAEL256 deriving Show
data CipherTwofish = CipherTwofish deriving Show
data CipherTwofish128 = CipherTwofish128 deriving Show
data CipherArcfour = CipherArcfour deriving Show
data CipherDES = CipherDES deriving Show

instance CipherType CipherIDEA where cipherTypeToC _ = #const GCRY_CIPHER_IDEA
instance CipherType Cipher3DES where cipherTypeToC _ = #const GCRY_CIPHER_3DES
instance CipherType CipherCast5 where cipherTypeToC _ = #const GCRY_CIPHER_CAST5
instance CipherType CipherBlowfish where cipherTypeToC _ = #const GCRY_CIPHER_BLOWFISH
instance CipherType CipherAES where cipherTypeToC _ = #const GCRY_CIPHER_AES
instance CipherType CipherAES128 where cipherTypeToC _ = #const GCRY_CIPHER_AES128
instance CipherType CipherAES192 where cipherTypeToC _ = #const GCRY_CIPHER_AES192
instance CipherType CipherAES256 where cipherTypeToC _ = #const GCRY_CIPHER_AES256
instance CipherType CipherRIJNDAEL where cipherTypeToC _ = #const GCRY_CIPHER_RIJNDAEL
instance CipherType CipherRIJNDAEL128 where cipherTypeToC _ = #const GCRY_CIPHER_RIJNDAEL128
instance CipherType CipherRIJNDAEL192 where cipherTypeToC _ = #const GCRY_CIPHER_RIJNDAEL192
instance CipherType CipherRIJNDAEL256 where cipherTypeToC _ = #const GCRY_CIPHER_RIJNDAEL256
instance CipherType CipherTwofish where cipherTypeToC _ = #const GCRY_CIPHER_TWOFISH
instance CipherType CipherTwofish128 where cipherTypeToC _ = #const GCRY_CIPHER_TWOFISH128
instance CipherType CipherArcfour where cipherTypeToC _ = #const GCRY_CIPHER_ARCFOUR
instance CipherType CipherDES where cipherTypeToC _ = #const GCRY_CIPHER_DES 

{-data CipherMode
	= ModeNone
	| ModeECB
	| ModeCFB
	| ModeCBC
	| ModeStream
	| ModeOFB
	| ModeCTR
	deriving (Show,Eq)-}

data ModeNone = ModeNone deriving Show
data ModeECB = ModeECB deriving Show
data ModeCFB = ModeCFB deriving Show
data ModeCBC = ModeCBC deriving Show
data ModeStream = ModeStream deriving Show
data ModeOFB = ModeOFB deriving Show
data ModeCTR = ModeCTR deriving Show

instance CipherMode ModeNone where cipherModeToC _ = #const GCRY_CIPHER_MODE_NONE
instance CipherMode ModeECB where cipherModeToC _ = #const GCRY_CIPHER_MODE_ECB
instance CipherMode ModeCFB where cipherModeToC _ = #const GCRY_CIPHER_MODE_CFB
instance CipherMode ModeCBC where cipherModeToC _ = #const GCRY_CIPHER_MODE_CBC
instance CipherMode ModeStream where cipherModeToC _ = #const GCRY_CIPHER_MODE_STREAM
instance CipherMode ModeOFB where cipherModeToC _ = #const GCRY_CIPHER_MODE_OFB
instance CipherMode ModeCTR where cipherModeToC _ = #const GCRY_CIPHER_MODE_CTR

data CipherFlag
	= Secure
	| EnableSync
	| CBC_CTS
	| CBC_MAC
	deriving (Show,Eq)

flagToC :: CipherFlag -> CUInt
flagToC Secure = #const GCRY_CIPHER_SECURE
flagToC EnableSync = #const GCRY_CIPHER_ENABLE_SYNC
flagToC CBC_CTS = #const GCRY_CIPHER_CBC_CTS
flagToC CBC_MAC = #const GCRY_CIPHER_CBC_MAC
