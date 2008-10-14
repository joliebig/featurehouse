{-# OPTIONS_GHC -O2 #-}

-- | This module implements RSA encryption with PKCS1 padding.

module Codec.Encryption.PKCS1
	(encrypt
	,decrypt
	,PublicKey(..)
	,PrivateKey(..)
	,PrivateKeyComplex(..)
	,PrivateKeySimple(..)
	,os2ip
	,i2osp) where

import Data.Bits
import Data.ByteString as BS hiding (take,split,zipWith)
import Data.ByteString.Char8 as BSC hiding (take,split,zipWith)
import System.Random

-- | The public key is contained in a certificate.
data PublicKey = PublicKey
	{publicN :: Integer -- ^ The modulus
	,publicE :: Integer -- ^ Public exponent
	} deriving (Show,Eq)

newtype PrivateKey = PrivateKey (Either PrivateKeySimple PrivateKeyComplex)

-- | This private key variant takes longer to decrypt.
data PrivateKeySimple = PrivateKeySimple
	{privateN :: Integer	-- ^ The modulus
	,privateD :: Integer	-- ^ Private exponent
	} deriving (Show,Eq)

-- | This private key variant is considerably faster.
data PrivateKeyComplex = PrivateKeyComplex
	{privateN' :: Integer	-- ^ The modulus
	,privateD' :: Integer	-- ^ The private exponent
	,privateP :: Integer	-- ^ The first prime number
	,privateQ :: Integer	-- ^ The second prime number
	,privateU :: Integer	-- ^ Go read a book about it for god sake
	} deriving (Show,Eq)

-- | Encrypts a ByteString using RSA with PKCS1 padding
encrypt :: RandomGen g => Int	-- ^ Size of the modulus in bytes
	-> PublicKey		-- ^ The public key to use
	-> g			-- ^ The padding is random, so we just need this
	-> ByteString		-- ^ The string to encrypt
	-> ByteString
encrypt sz key gen str = BS.concat $ zipWith (rsaPkcs1encrypt sz key) (gens gen) (splitBlock (sz-11) str)

-- | Decrypts a ByteString using RSA with PKCS1 padding. If the ByteString
--   couldn't be decrypted, nothing is returned.
decrypt :: Int		-- ^ Size of the modulus in bytes
	-> PrivateKey	-- ^ The private key to use
	-> ByteString	-- ^ The string to decrypt
	-> Maybe ByteString
decrypt sz key str = fmap BS.concat $ sequence (fmap (rsaPkcs1decrypt sz key) (splitBlock sz str))

gens :: RandomGen g => g -> [g]
gens g = let (g1,g2) = split g in g1:gens g2

rsaPkcs1encrypt :: RandomGen g => Int -> PublicKey -> g -> ByteString -> ByteString
rsaPkcs1encrypt sz key gen str = let
	ps = take (sz - BS.length str - 3) (fmap fromIntegral $ randomRs (1,255) gen)
	em = BS.append (BS.pack $ 0x00:0x02:(ps++[0x00])) str
	in i2osp sz $ rsaep key (os2ip em)

rsaPkcs1decrypt :: Int -> PrivateKey -> ByteString -> Maybe ByteString
rsaPkcs1decrypt sz key str = let
	em = i2osp sz $ rsadp key (os2ip str)
	(begin,rest) = BS.splitAt 2 em
	(ps,msg) = BS.break (==0) rest
	in if begin == BS.pack [0x00,0x02] && BS.length ps >= 8 && not (BS.null msg)
		then Just (BS.tail msg)
		else Nothing

rsaep :: PublicKey -> Integer -> Integer
rsaep key m = expmod m (publicE key) (publicN key)

rsadp :: PrivateKey -> Integer -> Integer
rsadp (PrivateKey k) = either rsadpSimple rsadpComplex k

-- inefficient
rsadpSimple :: PrivateKeySimple -> Integer -> Integer
rsadpSimple key c = expmod c (privateD key) (privateN key)

-- efficient
rsadpComplex :: PrivateKeyComplex -> Integer -> Integer
rsadpComplex key c = let
	m1 = expmod c ((privateD' key) `mod` ((privateP key)-1)) (privateP key)
	m2 = expmod c ((privateD' key) `mod` ((privateQ key)-1)) (privateQ key)
	h  = (m1 - m2)*(privateU key) `mod` (privateP key)
	in m2 + (privateQ key)*h 

expmod :: Integer -> Integer -> Integer -> Integer
expmod a x m = expmod' a x m 1

expmod' :: Integer -> Integer -> Integer -> Integer -> Integer
expmod' _ 0 _ res = res
expmod' a x m res = expmod' ((a*a) `mod` m) (x `div` 2) m (if odd x
	then (res*a) `mod` m
	else res)

-- | Converts a block of bytes into a number
os2ip :: (Bits a,Integral a) => ByteString -> a
os2ip bs = let
	len = BS.length bs
	in fst $ BS.foldl' (\(cur,p) w -> 
		(cur + shiftL (fromIntegral w) (8*(len-1-p)),p+1)
		) (0,0) bs

-- | Converts a number into a block of bytes
i2osp :: (Bits a,Integral a) => Int -> a -> ByteString
i2osp len num = fst $ BS.unfoldrN len (\p -> if p >= 0
	then Just (fromIntegral $ shiftR num (p*8),p-1)
	else Nothing) (len-1)

splitBlock :: Int -> ByteString -> [ByteString]
splitBlock sz str
	| BS.null str = []
	| otherwise   = let (x,xs) = BS.splitAt sz str in x:splitBlock sz xs

{-# INLINE os2ip #-}
{-# INLINE i2osp #-}
{-# SPECIALIZE os2ip :: ByteString -> Integer #-}
{-# SPECIALIZE i2osp :: Int -> Integer -> ByteString #-}
{-# SPECIALIZE encrypt :: Int -> PublicKey -> StdGen -> ByteString -> ByteString #-}
{-# SPECIALIZE rsaPkcs1encrypt :: Int -> PublicKey -> StdGen -> ByteString -> ByteString #-}
