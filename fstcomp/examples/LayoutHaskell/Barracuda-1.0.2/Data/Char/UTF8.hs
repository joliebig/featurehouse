module Data.Char.UTF8 (toUTF8,fromUTF8) where

import Control.Monad
import Data.Char
import Data.Bits

toUTF8 :: String -> String
toUTF8 [] = []
toUTF8 (x:xs)
	| ord x <= 0x007F = x:toUTF8 xs
	| ord x <= 0x07FF = chr (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
			    chr (0x80 .|. (ord x .&. 0x3F)):
			    toUTF8 xs
	| otherwise       = chr (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
			    chr (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
			    chr (0x80 .|. (ord x .&. 0x3F)):
			    toUTF8 xs

fromUTF8 :: String -> Maybe String
fromUTF8 [] = Just []
fromUTF8 (all@(x:xs))
	| ord x<=0x7F = fromUTF8 xs >>= return.(x:)
	| ord x<=0xBF = Nothing
	| ord x<=0xDF = twoBytes all
	| ord x<=0xEF = threeBytes all
	| otherwise   = Nothing
	where
	twoBytes (x1:x2:xs)
		= fromUTF8 xs >>= return.(chr (
			((ord x1 .&. 0x1F) `shift` 6) .|.
			 (ord x2 .&. 0x3F)):)
	twoBytes _ = Nothing

	threeBytes (x1:x2:x3:xs) = fromUTF8 xs >>= return.(chr (
		((ord x1 .&. 0x0F) `shift` 12) .|.
		((ord x2 .&. 0x3F) `shift` 6) .|.
		 (ord x3 .&. 0x3F)):)
	threeBytes _ = Nothing
