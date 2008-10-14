module KeyName(
    stringToKeys,
    keyCanon,
    keysToString,
    showKeyInfo,
    buildKeyTable,
    getKeyHelpTable
    ) where

import Char
import ConfigFile
import GenUtil
import KeyHelpTable
import List
import Screen(Key(..))

stringToKeys :: String -> [Key]
stringToKeys "" = []
stringToKeys ('\\':'x':a:b:cs) = (dh [a,b]) : stringToKeys cs  where
    dh x = case readHex x of
        Nothing -> KeyUnknown 0
        Just k -> KeyChar (chr k)
--stringToKeys ('<':c:'-':x:'>':cs) | c `elem` "cC" = KeyChar (ord (toLower x) - 0x40) : stringToKeys cs
stringToKeys ('<':cs) = let (n,r) = span (/= '>') cs in
     case lookup ((map toLower $ filter (not . isSpace) n)) ftl of
	Just k -> k
	Nothing -> KeyUnknown 0
      : stringToKeys (drop 1 r)
stringToKeys (c:cs) = KeyChar c : stringToKeys cs


namedKeyTableInt = [
    ("NUL",0),
    ("Backspace",8),
    ("BS",8),
    ("Tab",9),
    ("Enter",13),
    ("Enter",10),
    ("NL",10),
    ("CR",13),
    ("Return",13),
    ("ESC",27),
    ("Space",32),
    ("LT",60),
    ("Backslash",92),
    ("SingleQuote",39),
    ("DoubleQuote",34),
    ("Bar",124),
    ("Backspace",127)
    ]

namedKeyTableKey = [
    ("Up",KeyUp),
    ("Down",KeyDown),
    ("Left",KeyLeft),
    ("Right",KeyRight),
    ("Help",KeyHelp),
    ("Undo",KeyUndo),
    ("Insert",KeyIC),
    ("Home",KeyHome),
    ("End",KeyEnd),
    ("PageUp",KeyPPage),
    ("PageDown",KeyNPage),
    ("Delete",KeyDC),
    ("Enter",KeyEnter),
    ("Print",KeyPrint),
    ("Backspace",KeyBackspace)
    ]


fTable = map (\n -> ('F':show n,KeyF n)) [0..31]

cTable = map (\n -> ("C-" ++ [chr (n + 64)], KeyChar (chr n))) [0..31]

ft,ftl :: [(String,Key)]
ft = fTable ++ namedKeyTableKey ++ map (\(x,y) -> (x, KeyChar (chr y))) namedKeyTableInt ++ cTable
ftl = map (\(x,y) -> (map toLower x,y)) ft

kg :: [[Key]]
kg = transitiveGroup $ map (snub . map snd) (groupBy (\(x,_) (y,_) -> x == y)
	(sort ftl))

lkg :: Key -> [Key]
lkg k = case lookup k kgm of
        Just g -> g
        Nothing -> [k]
kgm = concatMap (\xs -> map (\x -> (x,xs)) xs) kg

transitiveGroup :: Eq a => [[a]] -> [[a]]
transitiveGroup gs = tg [] gs where
    tg x [] = x
    tg gs (g:rg) = tg (f gs g) rg
    f [] g = [g]
    f (x:xs) g | isConjoint g x = nub (x ++ g):xs
    f (x:xs) g = x:f xs g

showKeyInfo :: String
showKeyInfo = unlines $ (buildTableRL $ map (\(x,y) -> (x,show y)) ft) ++ sort (map (show . sort) kg)


keyCanon :: Key -> Key
keyCanon k | ((y:_):_) <- [z| z <- kg, k `elem` z] = y
keyCanon k = k

keysToString ks = concatMap ck ks where
    ck c | (x:_) <- [n| (n,k) <- ft, k == c] = '<' : (x ++ ">")
    ck (KeyChar c) | ord c >= 0x80 && ord c < 0xa0 = "<" ++ show c ++ ">"
    ck (KeyChar c) = [c]
    ck k = "<KeyUnknown:" ++ show k ++ ">"


type Action = String



buildKeyTable :: IO [(Key,Action)]
buildKeyTable = do
    cl <- configLookupList "bind"
    let fl = concatMap ((\x -> do (a:b:_) <- return x ; return (a,b)) . words) cl
    return  (concatMap mk fl) where
        mk (k,a) | (k:_) <- stringToKeys k = map (rtup a) (lkg k)
        mk _ = []


{-# NOTINLINE getKeyHelpTable #-}
getKeyHelpTable :: (Int,Int) -> IO String
getKeyHelpTable (_,_) = buildKeyTable >>= \kt -> let
    tl = concatMap f (keyHelpTable gk)
    f (Right (x,y)) = [(x,y)]
    f (Left x) = [("",""),(x ++ ":","")]
    gk :: String -> String
    gk s = maybe "" id $ lookup s m
    m :: [(String,String)]
    m = [ (fst (head xs),ks (snub (snds xs))) | xs <- groupFst (sort [ (y,x) | (x,y) <- kt])]
    ks ks = concatInter ", " (snub $ map (\x -> keysToString [x]) ks)

      in return $ unlines (bTableRL tl)

bTableRL :: [(String,String)] -> [String]
bTableRL ps = map f ps where
    f ("","") = ""
    f (x,"") = x
    f ("",y) = y
    f (x,y) = replicate (bs - length x) ' ' ++ x ++ replicate 4 ' ' ++ y
    bs = maximum (map (length . fst) [p | p <- ps, not (null (snd p))])

