module Charset (encodeCharset, decodeCharset) 
where

import Data.Char (chr, ord)
import Data.List (isPrefixOf, tails)
import GHC.Word (Word8)

import Iconv
import UTF8

encodeCharset :: Maybe String -> String -> String
encodeCharset Nothing str = encodeUTF8 str
encodeCharset (Just charset) str =
    case iconv "UTF-8" charset (map charToWord8 $ encodeUTF8 str) of
         Success cs -> map word8ToChar cs
         _ -> str
    

decodeCharset :: String -> String
decodeCharset str =
    if stringContains "\ESC$B" str
       then decodeEUC str
       else decodeUTF8 str
  where
  stringContains :: String -> String -> Bool
  stringContains s = any (s `isPrefixOf`) . tails

-- decodeCharset :: Maybe String -> String -> String
-- decodeCharset Nothing str = decodeUTF8 str
-- decodeCharset (Just charset) str =
--     case iconv charset "UTF-8" (map charToWord8 str) of
--          Success cs -> decodeUTF8 $ map word8ToChar cs
--          _ -> str

decodeEUC :: String -> String
decodeEUC str =
    case iconv "ISO-2022-JP" "UTF-8" (map charToWord8 str) of
         Success cs -> decodeUTF8 $ map word8ToChar cs
         _ -> str

word8ToChar :: Word8 -> Char
word8ToChar = chr . fromInteger . toInteger

charToWord8 :: Char -> Word8
charToWord8 = fromInteger . toInteger . ord
