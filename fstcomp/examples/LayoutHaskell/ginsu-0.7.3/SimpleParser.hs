--  $Id: SimpleParser.hs,v 1.1 2008-10-14 18:01:39 apel Exp $
-- arch-tag: b8b68a0d-1f68-48d3-af44-1704404ef004

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

module SimpleParser where

import Char
import Monad
import List

infixr 1 <|>

-- very simple parser combinators with failure but limited non-determinism.
-- designed for parsing single lines or simple expressions.
-- advantages: pure haskell 98, simple.


newtype GenParser c a = MkP ([c] -> Maybe (a,[c]))
type Parser a = GenParser Char a



instance Monad (GenParser c) where
    return a = MkP (\s -> (Just (a,s)))
    (MkP p) >>= q = MkP $ \s ->  (maybe Nothing (\(v,s') -> app (q v) s') (p s))
    fail _ = MkP $ \_ ->  Nothing

instance Functor (GenParser c) where
    fmap = liftM


instance MonadPlus (GenParser c) where
    mzero = MkP (\_ -> Nothing)
    mplus = (<|>)


app (MkP fn) s = fn s

char :: Char -> Parser Char
char c = sat (== c)

eof :: GenParser c ()
eof = MkP eof' where
    eof' [] = Just ((),[])
    eof' _ = Nothing

oneOf cs = sat (`elem` cs)
noneOf cs = sat (`notElem` cs)

between o c p = o >> p >>= \v -> c >> return v

{-# SPECIALIZE parseSome :: Int -> GenParser c [c] #-}
parseSome :: Integral a => a -> GenParser c [c]
parseSome count = MkP f where
    f xs | length xs >= (fromIntegral count) = Just (splitAt (fromIntegral count) xs)
    f _ = Nothing

{-# SPECIALIZE parseExact :: String -> GenParser Char () #-}
parseExact :: Eq c => [c] -> GenParser c ()
parseExact x = MkP f where
    f xs | x `isPrefixOf` xs = Just ((),drop (length x) xs)
    f _ = Nothing

parseRest = MkP (\xs -> Just (xs,[]))

option x p = p <|> return x
skipOption p = option () $ liftM (const ()) p
choice ps = foldl (<|>) mzero ps

anyChar = satisfy (const True)

upper = satisfy isUpper
lower = satisfy isLower
alphaNum = satisfy isAlphaNum
digit = satisfy isDigit
newline = char '\n'
space = satisfy isSpace
spaces = skipMany space


sat p = MkP f where
    f (x:xs) | p x = Just (x,xs)
    f _ = Nothing

satisfy = sat

p <|> q = MkP $ \s -> maybe (app q s) Just (app p s)

many :: GenParser c a -> GenParser c [a]
many p = (p >>= \x -> many p >>= \xs -> return (x:xs)) <|> return []

many1 :: GenParser c a -> GenParser c [a]
many1 p = p >>= \x -> many p >>= \xs -> return (x:xs)

whitespace = skipMany (sat isSpace)

token p = p >>= \x -> whitespace >> return x

skipMany p = (p >> skipMany p) <|> return ()

number :: Parser Int
number = token (liftM read $ many1 (sat isDigit))

word :: Parser String
word = token $ (many1 (sat isAlpha))

exactWord s = do
    w <- word
    if w == s then return w else mzero

{-# SPECIALIZE parser :: GenParser c a -> [c] -> Maybe a #-}
{-# SPECIALIZE parser :: GenParser c a -> [c] -> IO a #-}
{-# SPECIALIZE parser :: GenParser Char a -> String -> Maybe a #-}
{-# SPECIALIZE parser :: GenParser Char a -> String -> IO a #-}

parser :: Monad m => GenParser c a -> [c] -> m a
parser (MkP fn) s = case fn s of
    Just (v,[]) -> return v
    _ -> fail "parser failed"

{-
parseIO p xs = case parser p xs of
    Just v -> return v
    Nothing -> ioError $ userError "parse error"
-}


{-
parse_file = do
    whitespace
    u <- number
    v <- number
    exactWord "done"
    return (u,v)

main = do
    c <- getContents
    let v =  parser parse_file c
    print v

-}
