module Tests.QuickCheck.Encryption (testByteStringEncrypt) where

import Control.Monad (replicateM)
import Data.ByteString
import Data.Word
import Network.AdHoc.Encryption
import Test.QuickCheck

genWord8 :: Gen Word8
genWord8 = fmap fromIntegral (choose (0,255)::Gen Integer)

instance Arbitrary ByteString where
	arbitrary = sized $ \sz -> fmap pack $ replicateM (sz*100) genWord8
	coarbitrary _ = id

genDESKey :: Gen Word64
genDESKey = do
	r <- rand
	let (key,_) = generateDESKey r
	return key

genIV :: Gen Word64
genIV = fmap fromIntegral (choose (0,2^64-1)::Gen Integer)

encryptDecrypt :: (Arbitrary a,Encryptable a,Eq a) => a -> Property
encryptDecrypt obj = forAll genDESKey $ \key ->
	forAll genIV $ \iv -> 
	decrypt key (encrypt key iv obj) == Just obj

testByteStringEncrypt :: ByteString -> Property
testByteStringEncrypt = encryptDecrypt
