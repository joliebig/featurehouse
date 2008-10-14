{-# OPTIONS -fglasgow-exts #-}

module UArrayParser where

import Data.Int
import Data.Word
import Data.Array.MArray
import Data.Array.Base
import Data.Bits
import Char(chr)
--import BinaryParser

{- UArrayParser is supposed to be optimised for speed.
   I think it could be much faster than it is now, but I have already reached
   speed that satisfies me.

   Ideas for improvement:
     - rewrite word* methods so they don't use so much monadic stuff
	(I wonder if GHC already inlines it?)
     - use unsafeAt
     - move right index bound to UArrayParser function parameter
-}

newtype UArrayParser a = UArrayParser (UArray Int Word8 -> Int -> (# Int, a #))

runUArrayParser :: Monad m => UArrayParser a -> UArray Int Word8 -> m a
runUArrayParser (UArrayParser parser) arr =
    case parser arr (fst (bounds arr)) of
	(# pos, x #) ->
	    if pos < 0
		then fail ("UArrayParser: no parse " ++ show (map (chr . fromIntegral) $ elems arr))
		else (return x)

instance Monad UArrayParser where
    return x = UArrayParser (\_ pos -> (# pos, x #))

    (UArrayParser m) >>= f =
	UArrayParser (\a pos ->
			case m a pos of
			    (# pos', x #) ->
				if pos' < 0
				    then (# pos', undefined #)
				    else
					let (UArrayParser m2) = f x in
					m2 a pos')

    fail _ = UArrayParser (\_ _ -> (# -1, undefined #))

    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    {-# INLINE fail #-}

instance Functor UArrayParser where
    fmap f m = m >>= return . f

    {-# INLINE fmap #-}

ensureBytes :: Int -> UArrayParser ()
ensureBytes n =
    UArrayParser (\arr pos ->
		    if pos+n-1 > snd (bounds arr)
			then (# -1, undefined #)
			else (# pos, () #))

dropBytes :: Int -> UArrayParser ()
dropBytes n =
    UArrayParser (\arr pos ->
		    if pos+n-1 > snd (bounds arr)
			then (# -1, undefined #)
			else (# pos + n , () #))

unsafeByte :: UArrayParser Word8
unsafeByte =
    UArrayParser (\arr pos -> (# pos+1, arr ! pos #))

unsafeGenericByte :: Integral n => UArrayParser n
unsafeGenericByte = do
    b <- unsafeByte
    return $! fromIntegral b

--instance BinaryParser UArrayParser where
byte = ensureBytes 1 >> unsafeByte

bytes n = do
    ensureBytes n
    UArrayParser (\arr pos ->
        let buf = listArray (0, n-1) [ arr!i | i <- [pos..pos+n-1] ] in
        buf `seq` (# pos+n, buf #))

word16 = do
    ensureBytes 2
    b1 <- unsafeGenericByte
    b0 <- unsafeGenericByte
    return $!
        b1 `shiftL` 8 .|.
        b0

word32 = do
    ensureBytes 4
    b3 <- unsafeGenericByte
    b2 <- unsafeGenericByte
    b1 <- unsafeGenericByte
    b0 <- unsafeGenericByte
    return $!
        b3 `shiftL` 24 .|.
        b2 `shiftL` 16 .|.
        b1 `shiftL`  8 .|.
        b0

word64 = do
    ensureBytes 8
    b7 <- unsafeGenericByte
    b6 <- unsafeGenericByte
    b5 <- unsafeGenericByte
    b4 <- unsafeGenericByte
    b3 <- unsafeGenericByte
    b2 <- unsafeGenericByte
    b1 <- unsafeGenericByte
    b0 <- unsafeGenericByte
    return $!
        b7 `shiftL` 56 .|.
        b6 `shiftL` 48 .|.
        b5 `shiftL` 40 .|.
        b4 `shiftL` 32 .|.
        b3 `shiftL` 24 .|.
        b2 `shiftL` 16 .|.
        b1 `shiftL`  8 .|.
        b0

asciiz =
    UArrayParser (\arr pos ->
        let (_, maxPos) = bounds arr in
        case findInArray 0 arr pos maxPos of
            Nothing -> (# -1, undefined #)
            Just p  -> (# p+1, subArray arr pos (p-1) 0 #))

eof = UArrayParser (\arr pos ->
        (# if pos <= snd (bounds arr) then -1 else pos, () #))

atEof = UArrayParser (\arr pos ->
        (# pos, if pos <= snd (bounds arr) then False else True #))

times nn (UArrayParser f) =
    UArrayParser (\arr pos -> g nn [] arr pos)
  where
    g 0 acc _   pos = (# pos, reverse acc #)
    g n acc arr pos =
        let (# pos', x #) = f arr pos in
        if pos' < 0
            then (# pos', undefined #)
            else g (n-1) (x:acc) arr pos'

-- instance BinaryParser UArrayParser

byte    :: UArrayParser Word8
word16  :: UArrayParser Word16
word32  :: UArrayParser Word32
word64  :: UArrayParser Word64
int8    :: UArrayParser Int8
int16   :: UArrayParser Int16
int32   :: UArrayParser Int32
int64   :: UArrayParser Int64
bytes   :: Int -> UArrayParser (UArray Int Word8)
asciiz :: UArrayParser (UArray Int Word8)
eof	    :: UArrayParser ()
atEof   :: UArrayParser Bool
times   :: Int -> UArrayParser a -> UArrayParser [a]

int8  = fmap fromIntegral byte
int16 = fmap fromIntegral word16
int32 = fmap fromIntegral word32
int64 = fmap fromIntegral word64

-- ArrayLib stuff

subArray :: (Integral i, IArray a e, Ix i, IArray a e) => a i e -> i -> i -> i -> a i e
subArray array from to from' =
    listArray
	(from', from' + fromIntegral (rangeSize (from, to)) - 1)
	[ array ! i | i <- range (from, to) ]

findInArray e arr pos maxPos
    | pos > maxPos	= Nothing
    | (arr ! pos) == e  = Just pos
    | otherwise		= findInArray e arr (pos+1) maxPos


restrictUArray :: i -> i -> UArray i e -> UArray i e
restrictUArray ns ne (UArray _ _ ba) = UArray ns ne ba



