module Charset(stringToBytes, bytesToString, charsetSetup, csHPutStr) where

import UTF8
import System
import Char
import Word(Word8)
import System.IO.Unsafe
import List
import EIO
import ErrorLog
import Control.Monad.Error
import Data.IORef
import MyLocale

-- arch-tag: 7ca73986-7350-4bbf-8a7e-2331a5fe94d6

{-# NOINLINE stb_r #-}
{-# NOINLINE bts_r #-}
stb_r = unsafePerformIO $ newIORef toLatin1
bts_r = unsafePerformIO $ newIORef fromSingleByte

{-# INLINE stringToBytes #-}
stringToBytes :: String -> [Word8]
stringToBytes s = f s where
    f = unsafePerformIO $ readIORef stb_r

{-# INLINE bytesToString #-}
bytesToString :: [Word8] -> String
bytesToString s = f s where
    f = unsafePerformIO $ readIORef bts_r


charMap = [
    (["UTF8"],(toUTF,fromUTF)),
    (["ASCII", "ANSIX341968"],(toAscii,fromSingleByte)),
    (["LATIN1","ISO88591"],(toLatin1,fromSingleByte)) ]


normalize s = map toUpper . filter isAlphaNum $ s

charsetSetup :: Maybe String -> IO ()
charsetSetup (Just s) = case [x| (al ,x) <- charMap, normalize s `elem` al ] of
    ((a,b):_) -> writeIORef stb_r a >> writeIORef bts_r b
    _ -> return ()
charsetSetup Nothing = do
    --ts <- getCharset
    es <- tryMapM id [getCharset, getEnv "LC_CTYPE", getEnv "LANG", return "LATIN1"]
    let (cn,(a,b)) = head [(head al,x)| e <- es, (al ,x) <- charMap,  any (`isSuffixOf` (normalize e)) al ]
    putLog LogInfo ("chose charset " ++ cn ++ " via " ++ show es)
    writeIORef stb_r a >> writeIORef bts_r b



toAscii :: String -> [Word8]
toAscii s = map f s where
    f x | ord x > 127 = fromIntegral (ord '?')
    f x = fromIntegral $ ord x

toLatin1 :: String -> [Word8]
toLatin1 s = map f s where
    f x | ord x > 255 = fromIntegral (ord '?')
    f x = fromIntegral $ ord x

fromSingleByte :: [Word8] -> String
fromSingleByte = map (chr . fromIntegral)



csHPutStr h s = do
    conv <- readIORef stb_r
    putRaw h (conv s)


