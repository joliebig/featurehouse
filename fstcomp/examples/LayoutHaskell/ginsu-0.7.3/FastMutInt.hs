{-# OPTIONS_GHC -fglasgow-exts #-}
module FastMutInt(
	FastMutInt,
        newFastMutInt,
	readFastMutInt,
        writeFastMutInt
  ) where


import GHC.Base
import GHC.IOBase
import Foreign.Storable

data FastMutInt = FastMutInt (MutableByteArray# RealWorld)

sSIZEOF_HSINT = sizeOf (undefined :: Int)

newFastMutInt :: IO FastMutInt
newFastMutInt = IO $ \s ->
  case newByteArray# size s of { (# s, arr #) ->
  (# s, FastMutInt arr #) }
  where I# size = sSIZEOF_HSINT

{-# INLINE readFastMutInt  #-}
readFastMutInt :: FastMutInt -> IO Int
readFastMutInt (FastMutInt arr) = IO $ \s ->
  case readIntArray# arr 0# s of { (# s, i #) ->
  (# s, I# i #) }

{-# INLINE writeFastMutInt  #-}
writeFastMutInt :: FastMutInt -> Int -> IO ()
writeFastMutInt (FastMutInt arr) (I# i) = IO $ \s ->
  case writeIntArray# arr 0# i s of { s ->
  (# s, () #) }

