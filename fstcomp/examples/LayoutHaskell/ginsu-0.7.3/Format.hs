--  $Id: Format.hs,v 1.1 2008-10-14 18:01:40 apel Exp $
-- arch-tag: 072f88a5-e441-4131-8934-f3a4a3aa0c12

-- Copyright (c) 2002 John Meacham (john@foo.net)
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


-- | General text formatting routines, similar to C printf-style formatting.
--
-- > General pattern format
-- > '%' '%'                                             output literal '%'
-- > '%' flags width '.' precision '\'' string '\''      always print string as is.
-- > '%' flags width '.' precision formatChar            passed to class instance.
-- > '%' '(' sub1, sub2, ... ')' flags width '.' precision formatChar  subpatterns
--
-- > Flags is zero or more of the following
-- > ' ' a blank should be left before positive numbers
-- > '#' value should be printed in alternate form
-- > '0' value should be zero padded
-- > '-' value should be left adjusted
-- > '+' value should always have a sign
-- > '\'' numerical values should be grouped
--
-- > formatChar is a character affecting formatting
-- > '/' always format as if by the following translation
-- >     fmt "%fx.yv" [fa x] -> fmt "%fx.ys" [fa (show x)]
-- > 'l' format as list
-- > 's' print list compactly, similar to %(%c)l
-- > 'x' hexadecimal
-- > 'i' integral
-- > 'b' binary
-- > 'o' octal
-- > 'c' print compactly, as is used in 's'


module Format(
    -- * The Format class
    Format(..),
    -- * Functions
    -- ** Formatting routines
    fmt, fmtS, fmtSs, errorf,
    -- ** Arguments
    fa, fS, fi, fs, ff,
    -- * Internals, useful for creating instances of Format
    Pattern(..),
    formatIntegral, formatString, formatShow,
    testFormat
    ) where

import Char(isDigit,ord)
import List(intersperse)

-- | the type of patterns
data Pattern = Pattern {
    patternSub :: [String], -- ^ subpatterns
    patternFlags :: [Char],
    patternWidth :: Maybe Int,
    patternPrecision :: Maybe Int,
    patternChar :: Char
    }

pattern :: Pattern
pattern = Pattern { patternSub = [], patternFlags = [],
    patternWidth = Nothing, patternPrecision = Nothing,
    patternChar = '/' }




type F = Pattern -> String

-- | class for use with 'fa'
class Show a => Format a where
    format :: a -> Pattern -> String
    format = formatShow

-- | format anything
fa :: (Format a) => a -> F
fa x p | patternChar p == '/' = formatShow x p
fa x p = format x p
-- | format showable
fS :: (Show a) => a -> F
fS x = fs (show x)
-- | format integer. specialization of 'fa', useful to resolve ambiguity.
fi :: Int -> F
fi = fa
-- | format string. specialization of 'fa', useful to resolve ambiguity.
fs :: String -> F
fs = fa
-- | format float. specialization of 'fa', useful to resolve ambiguity.
ff :: Float -> F
ff = fa



-- | main formatting routine.
-- @fmt \"%s has %i pets\" [fa \"John\", fi 3]@

fmt :: String -> [F] -> String
fmt ('%':'%':xs) fs = '%':fmt xs fs
fmt ('%':xs) (f:fs) =  case xs5 of
	('\'':cs) -> let (a,b) = gs "" cs in formatString a bp ++ fmt b fs
	(c:cs) -> f (bp {patternChar = c}) ++ fmt cs fs
	[] -> []
     where
    bp = pattern {patternSub = ss, patternFlags = flags, patternWidth = w, patternPrecision =  p}
    (flags,xs2) = span (`elem` "# 0-+'") xs
    (w,xs3) = grabNum xs2
    (p,xs4) = case xs3 of
	('.':xs) -> grabNum xs
	xs -> (Nothing, xs)
    (ss,xs5) = case xs4 of
	('(':xs) -> let (u,v) = span (/= ')') xs in ([u],tail v)
	xs -> ([],xs)
    grabNum xs = let (u,v) = span isDigit xs in if null u then (Nothing,v) else (Just (read u),v)
    gs x ('\'':'\'':cs) = gs ('\'':x) cs
    gs x ('\'':cs) = (reverse x,cs)
    gs x (c:cs) = gs (c:x) cs
    gs x [] = (reverse x,[])
fmt (x:xs) fs = x:fmt xs fs
fmt "" _ = ""

-- | format a single string
fmtS :: String -> String -> String
fmtS f x = fmt f [fs x]

-- | format a set of strings
fmtSs :: String -> [String] -> String
fmtSs f xs = fmt f (map fs xs)


-- | throws an error created by format
errorf :: String -> [F] -> a
errorf s fs = error $ fmt s fs

putBase :: Int -> Integer -> String
putBase base x = if null v then "0" else v where
    v = reverse (foo x)
    hex = "0123456789abcdef"
    foo 0 = ""
    foo x = let (u,v) = x `divMod` (toInteger base) in
	(hex !! fromIntegral v) : foo u


childFmt :: Pattern -> [F] -> String
childFmt (Pattern {patternSub = [s]})  = fmt s
childFmt _ = fmt "%/"

instance Format Float
instance Format ()
instance Format Int where format = formatIntegral
instance Format Integer where format = formatIntegral
instance (Format a, Format b) => Format (a,b) where
    format (x,y) p | patternChar p == 't' = format x p ++ format y p
    format x p = formatShow x p
instance Format a => Format [a] where
    format xs p | ( '#' `elem` patternFlags p) && patternChar p == 's' = "\""  ++ formatString (concatMap (flip format $ pattern {patternChar = 'c'}) xs) p ++ "\""
    format xs p | patternChar p == 's' = formatString ( concatMap (flip format $ pattern {patternChar = 'c'}) xs ) p
    format xs p | ( '#' `elem` patternFlags p) && patternChar p == 'l' = "[" ++ concat (intersperse ", " (map (\x -> childFmt p [fa x]) xs)) ++ "]"
    format xs p | patternChar p == 'l' = ( concat (map (\x -> childFmt p [fa x]) xs))
    format xs p = formatShow xs p

instance Format a => Format (Maybe a) where
    format (Just x) p = childFmt p [fa x]
    format  Nothing p = "<Nothing>"
instance Format Char where
    format x p | patternChar p == 'c' = x:[]
    format x p | patternChar p `elem` integralChars = formatIntegral (ord x) p
    format x p = formatShow x p
    --format x _  = show x

instance (Format a,Format b) => Format (Either a b) where
    format (Left x) p | ( '#' `elem` patternFlags p)  = "Left " ++ childFmt p [fa x]
    format (Right x) p | ( '#' `elem` patternFlags p)  = "Right " ++ childFmt p [fa x]
    format (Left x) p = childFmt p [fa x]
    format (Right x) p = childFmt p [fa x]

instance Format Bool where
    format True p | patternChar p == 'b' = "true"
    format False p | patternChar p == 'b' = "false"
    format v p  = formatShow v p


formatShow :: Show a => a -> Pattern -> String
formatShow x = formatString (show x)

integralChars = "xboiu"

formatIntegral :: Integral n => n -> Pattern -> String
formatIntegral v p@(Pattern {patternChar = 'x'}) = integralFmt p v 16
formatIntegral v p@(Pattern {patternChar = 'b'}) = integralFmt p v 2
formatIntegral v p@(Pattern {patternChar = 'o'}) = integralFmt p v 8
formatIntegral v p@(Pattern {patternChar = 'i'}) = integralFmt p v 10
formatIntegral v p@(Pattern {patternChar = 'u'}) = integralFmt p v 10
formatIntegral v p = formatShow v p

integralFmt p v base = pre ++ val where
    pos = if ' ' `elem` patternFlags p then " " else if '+' `elem` patternFlags p then "+" else ""
    pre = if v < 0 then "-" else pos
    val = putBase base (abs $ toInteger v)

formatString ::  String -> Pattern -> String
formatString s p@(Pattern {patternPrecision = Just x }) = formatString (take x s) (p {patternPrecision = Nothing})
formatString s (Pattern {patternWidth = Just w }) | length s >= w = s
formatString s (Pattern {patternFlags = pf, patternWidth = Just w} ) = if '-' `elem` pf then s ++ e else e ++ s where
    e = replicate (w - length s) ' '
formatString s _ = s


{-
fmtStr :: Bool -> Int -> String -> String
fmtStr _ 0 s = s
fmtStr False n s = take (abs n) s
fmtStr True n s | length s > (abs n) = take (abs n) s
fmtStr True n s | n > 0 =  replicate (n - length s) ' ' ++ s
fmtStr True n s | n < 0 = s ++ replicate ((negate n) - length s) ' '
-}


------------
-- test case
------------

concatInter p xs = concat (intersperse p xs)

nums :: [Integer]
nums = [ 0,  878987, -2830,  77, 0xabcdef]

twords = ["foo", "a", "fuzz"]

fmts = ["%8i", "%-+8i", "%b", "%o", "%x"]


tfmt :: String -> [F] -> String
tfmt f v = show (fmt f v) ++ "\t\t<- fmt \"" ++ f ++ "\" [" ++ fmt (concatInter ", " (replicate (length v) "%v")) v ++ "]" where
    fmtEsc ('%':xs) = '%':'%':fmtEsc xs
    fmtEsc (x:xs) = x:fmtEsc xs
    fmtEsc "" = ""
fmtsAp x = tfmt (unwords fmts) (replicate (length fmts) (fa x)) where
    --v = map (\(fe, f) -> "(" ++ fe ++ " " ++ f ++ ")") $ zip (map fmtEsc fmts) fmts
    fmtEsc ('%':xs) = '%':'%':fmtEsc xs
    fmtEsc (x:xs) = x:fmtEsc xs
    fmtEsc "" = ""


testFormat = do
    putStr $ unlines (map fmtsAp nums)
    --putStr $ unlines $ map (\x -> fmt "*%s*" [fa x]) [(f,fmt f [fs x]) | f <- ["%8s","%-8s","%s"], x <- twords]
    putStr $ unlines $ [tfmt f [fa x] | f <- ["%8s","%-8s","%s", "%#s", "%2.2s", "%l", "%#l", "%#(chr 0x%x)l", "%(-)l"], x <- twords]


