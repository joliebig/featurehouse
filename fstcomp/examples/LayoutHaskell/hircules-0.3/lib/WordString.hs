module WordString where

import Data.Char (isSpace)
import Data.List (isPrefixOf)

whead :: String -> String
whead str =
    let ws = words str in
    if null ws
       then str
       else head ws

wtail :: String -> String
wtail s = 
    let start = dropWhile isSpace s
	(_, rest) = break isSpace start in
    if null rest
       then ""
       else tail rest

wlast :: String -> String
wlast = last . words

wnth :: Int -> String -> String
wnth n cs | (n > 1 && n <= length ws) = ws !! (n-1)
  where
  ws = words cs
wnth _ _ = ""

-- splitColon :: String -> (String,String)
-- splitColon cs = (p1,p2)
--     where
--     (p1,p') = break (== ':') cs
--     p2 = if null p' then "" else tail p'

breakString :: (Eq a) => [a] -> [a] -> ([a],[a])
breakString _ [] = ([],[])
breakString glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], drop (length glue) rest)
    | otherwise = (x:piece, rest') where (piece, rest') = breakString glue xs 

startColon :: String -> Bool
startColon (':':_) = True
startColon _ = False

(+-+) :: String -> String -> String
"" +-+ s2 = s2
s1 +-+ "" = s1
s1 +-+ s2 = s1 ++ " " ++ s2

-- wbreak :: (String -> Bool) -> String -> (String, String)
-- wbreak 

-- | Split a list into pieces that were held together by glue.  Example:
--
-- > split ", " "one, two, three" ===> ["one","two","three"] 
split :: Eq a => [a] -- ^ Glue that holds pieces together
      -> [a]         -- ^ List to break into pieces
      -> [[a]]       -- ^ Result: list of pieces
split glue = split'
    where
    split' [] = []
    split' xs = piece : split' rest
        where (piece, rest) = breakString glue xs
--     dropGlue = drop (length glue)
