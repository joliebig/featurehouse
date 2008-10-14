module Regex where

import Char
import ConfigFile
import Control.Exception
import GenUtil
import Maybe
import Monad
import PackedString
import System.IO.Unsafe
import Text.Regex.Posix
import Text.Regex.Posix.String
import Text.Regex

subst :: String -> [String] -> String
subst "" _ = ""
subst ('$':'$':cs) xs  = '$':subst cs xs
subst ('$':c:cs) xs | isDigit c = f xs (ord c - ord '0') ++ subst cs xs where
    f (x:_) 0 = x
    f (_:xs) n = f xs (n - 1)
    f _ _ = ""
subst (c:cs) xs = c:subst cs xs


matches :: Regex -> String -> [[String]]
matches rx s = case unsafePerformIO (regexec rx s >>= fromWrapError) of
    Nothing -> []
    Just (_,v,r,xs) -> (v:xs):matches rx r


matchWords :: [(Regex,String,String)] -> String -> [(String,String)]
matchWords ((rx,a,b):rs) s = (map f $ matches rx s) ++ matchWords rs s where
    f xs = (subst a xs, subst b xs)
matchWords [] _ = []


buildMatchTable :: IO [(Regex,String,String)]
buildMatchTable = do
    hs <- configLookupList "apphook"
    let zs = catMaybes $ snds $ snubFst $ concatMap (f . simpleUnquote) (hs)
        f [n] = [(n,Nothing)]
        f [n,re,e] | Just rx <- compileRx re = [(n,Just (rxRegex rx, e, "$0"))]
        f [n,re,e,p] | Just rx <- compileRx re = [(n,Just (rxRegex rx, e, p))]
        f _ = []
    return zs


fromWrapError (Left r) = fail (show r)
fromWrapError (Right x) = return x

data Rx = Rx { rxString :: String, rxRegex :: Regex }

compileRx :: Monad m => String -> m Rx
compileRx re = liftM (Rx re) $ unsafePerformIO ( handle (\e -> return (fail $ show e)) (compile flags execBlank re' >>= fromWrapError >>= (return . return))) where
    flags = compExtended + ci + ml
    ci = if 'i' `elem` fl then compIgnoreCase else 0
    ml = if 'm' `elem` fl then compNewline else 0
    (fl,re') = ef re
    ef ('(':'?':cs) = let (a,b) = span (/= ')') cs in (a,drop 1 b)
    ef xs = ("",xs)


instance Show Rx where
    show (Rx s _) = s

matchRx re body = isJust (unsafePerformIO (regexec (rxRegex re) (unpackPS body) >>= fromWrapError))

