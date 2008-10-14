module Gale.Puff(
    Puff(..),
    Fragment(..),
    Signature(..),
    Key(..),
    FragmentList,
    HasFragmentList(..),
    Category(Category),
    readPuffs, writePuffs, emptyPuff,
    categoryHead, categoryCell,
    catParseNew, catShowNew,
    subCategory, fragmentData,
    getFragmentString, getFragmentStrings, getFragmentForceStrings, showPuff, showKey, showFragments,
    fragmentString, mergeFrags, getFragmentData, getFragmentTime,
    getAuthor, getSigner, hasFragment, hasFragmentString, emptyKey,
    getAllFragmentForceStrings,
    showSignature,
    f_keySigned,
    f_keyRedirect,
    f_keyMember,
    f_keyExpires,
    f_rsaModulus,
    f_rsaPrivateExponent,
    f_rsaPrivateCoefficient,
    f_rsaPrivatePrime,
    f_rsaPrivatePrimeExponent,
    f_rsaExponent,
    f_rsaBits,
    f_keyOwner,
    f_messageSender,
    f_securitySignature,
    f_messageBody,
    f_messageKeyword,
    f_idTime,
    f_messageId,
    f_questionReceipt,
    f_outGone,
    f_noticePresence,
    f_noticePresence',
    f_securityEncryption,
    f_questionKey,
    f_questionKey',
    f_answerKey',
    f_answerKeyError'
    ) where

import Atom
import Binary
import Char
import Control.Exception
import Data.Array.Unboxed
import EIO
import ErrorLog
import GenUtil
import Int(Int32)
import List
import Maybe(isJust)
import PackedString
import SHA1
import System.IO
import Time
import Word(Word8)

newtype Category = Category (String,String)
    deriving(Binary,Eq,Ord)

instance Show Category where
    show c = catShowNew c

type FragmentList = [(Atom,Fragment)]

data Puff = Puff {
    cats :: [Category],
    signature :: [Signature],
    fragments :: FragmentList
    }
data Fragment =
    FragmentText !PackedString
    | FragmentData !(UArray Int Word8)
    | FragmentTime !ClockTime
    | FragmentInt !Int32
    | FragmentNest FragmentList

data Signature =
    Unverifyable Key
    | Signed Key
    | Encrypted [String]

data Key = Key String FragmentList

emptyKey n = Key n [( f_keyMember, FragmentText nilPS)]

instance Eq Key where
    (==) (Key kn _) (Key kn' _) = kn == kn'
instance Ord Key where
    compare (Key kn _) (Key kn' _) = compare kn kn'


f_answerKeyError' = fromString "answer/key/error"
f_answerKey' = fromString "answer/key"
f_idTime = fromString "id/time"
f_keyExpires = fromString "key.expires"
f_keyMember = fromString "key.member"
f_keyOwner = fromString "key.owner"
f_keyRedirect = fromString "key.redirect"
f_keySigned = fromString "key.signed"
f_messageBody = fromString "message/body"
f_messageId = fromString "message.id"
f_messageKeyword = fromString "message.keyword"
f_messageSender = fromString "message/sender"
f_noticePresence = fromString "notice.presence"
f_noticePresence' = fromString "notice/presence"
f_outGone = fromString "out.gone"
f_questionKey = fromString "question.key"
f_questionKey' = fromString "question/key"
f_questionReceipt = fromString "question.receipt"
f_rsaBits = fromString "rsa.bits"
f_rsaExponent = fromString "rsa.exponent"
f_rsaModulus = fromString "rsa.modulus"
f_rsaPrivateCoefficient = fromString "rsa.private.coefficient"
f_rsaPrivateExponent = fromString "rsa.private.exponent"
f_rsaPrivatePrimeExponent = fromString "rsa.private.prime.exponent"
f_rsaPrivatePrime  = fromString "rsa.private.prime"
f_securityEncryption = fromString "security/encryption"
f_securitySignature = fromString "security/signature"

--------------------
-- Category Routines
--------------------

subCategory :: Category -> Category -> Bool
Category (x1,y1) `subCategory` Category (x2,y2) = x2 `isPrefixOf` x1 && y2 `isSuffixOf` y1

catParseNew :: String -> Category
catParseNew cs = Category (c,(drop 1) d) where
    (c,d) = span (/= '@') cs


catShowNew :: Category -> String
catShowNew (Category (x,"")) = x
catShowNew (Category (x,y)) = x ++ "@" ++ y

---------------------
-- working with puffs
---------------------

getAuthor p = maybe "unknown" id (getSigner p)

getSigner p = ss (signature p) where
	    ss (Signed (Key n _):_) = Just n
	    ss (Unverifyable (Key n _):_) = Just n
	    ss (_:sig) = ss sig
	    ss [] = Nothing

showSignature (Signed (Key n _)) = "Signed by " ++ n
showSignature (Unverifyable (Key n _)) = "Signed by " ++ n ++ " (unverified)"
showSignature (Encrypted xs) = "Encrypted to " ++ concatInter ", " xs

emptyPuff = Puff {cats = [], signature = [], fragments = []}

fragmentData :: Monad m => Atom -> FragmentList -> m (UArray Int Word8)
fragmentData s fl = case lookup s fl of
    Just (FragmentData xs) -> return xs
    _ -> fail $ "fragment not found: " ++ toString s

fragmentString :: Monad m => Atom -> FragmentList -> m PackedString
fragmentString s fl = case lookup s fl of
    Just (FragmentText xs) -> return xs
    _ -> fail $ "fragment not found: " ++ toString s

mergeFrags :: FragmentList -> FragmentList -> FragmentList
mergeFrags fla flb = fla ++ [f|f@(s,_) <- flb, s `notElem` fsts fla]


{-
getFragmentString :: Monad m => Puff -> String -> m String
getFragmentString (Puff {fragments = frags}) s = case lookup s frags of
    Just (FragmentText s) -> return s
    _ -> fail $ "fragment not found: " ++ s
-}

getFragmentStrings :: HasFragmentList fl => fl -> Atom -> [PackedString]
getFragmentStrings fl s = [v|(n,FragmentText v) <- getFragmentList fl, n == s]

getFragmentForceStrings :: HasFragmentList fl => fl -> Atom -> [PackedString]
getFragmentForceStrings fl s = concatMap f [v | (n,v) <- getFragmentList fl, n == s] where
    f (FragmentText t) = [t]
    f (FragmentData _) = []
    f (FragmentTime t) = [packString (show t)]
    f (FragmentInt i) = [packString (show i)]
    f (FragmentNest fl) = getFragmentForceStrings fl s

getAllFragmentForceStrings :: HasFragmentList fl => fl -> [PackedString]
getAllFragmentForceStrings fl  = concatMap f [v | (_,v) <- getFragmentList fl] where
    f (FragmentText t) = [t]
    f (FragmentData _) = []
    f (FragmentTime t) = [packString (show t)]
    f (FragmentInt i) = [packString (show i)]
    f (FragmentNest fl) = getAllFragmentForceStrings fl

showPuff (Puff {cats = s, fragments =  fl, signature = sigs}) = unwords (map catShowNew s) ++ "\n" ++ unlines (map showSignature sigs) ++ indentLines 2 (showFragments fl)
showFragments fl = concatMap (\(n,f) -> toString n ++ ": " ++ show f ++ "\n") fl

instance Show Puff where
    show = showPuff

instance Show Key where
    show = showKey

instance Show Fragment where
    show (FragmentText s) = show s
    --show (FragmentData xs) = "<DATA:" ++ show (rangeSize $ bounds xs)  ++ ":" ++ show (map (chr . fromIntegral) $ elems xs) ++ ">"
    show (FragmentData xs) = "<DATA:" ++ show (rangeSize $ bounds xs)  ++ ":" ++ sha1ShowHash (sha1 $ elems xs) ++ ">"
    show (FragmentTime ct) = show ct
    show (FragmentInt x) = show x
    show (FragmentNest fl) = "Nest:" ++ (indentLines 2 $ showFragments fl)


showKey :: Key -> String
showKey (Key n fl) = "Key: " ++ n ++ "\n" ++ (indentLines 4 $ showFragments fl)

getFragmentData :: (Monad m,HasFragmentList fl) => fl -> Atom -> m (UArray Int Word8)
getFragmentData fl s = case [xs | (s',FragmentData xs) <- getFragmentList fl, s' == s] of
    (s:_) -> return s
    [] -> fail $ "data fragment not found: " ++ toString s

getFragmentString :: (Monad m, HasFragmentList fl)  => fl -> Atom -> m PackedString
getFragmentString fl s = case [xs | (s',FragmentText xs) <- getFragmentList fl, s' == s] of
    (s:_) -> return s
    [] -> fail $ "text fragment not found: " ++ toString s

hasFragmentString fl s = isJust (getFragmentString fl s)

hasFragment fl s = s `elem` fsts (getFragmentList fl)

getFragmentTime :: (Monad m, HasFragmentList fl)  => fl -> Atom -> m ClockTime
getFragmentTime fl s = case [xs | (s',FragmentTime xs) <- getFragmentList fl, s' == s] of
    (s:_) -> return s
    [] -> fail $ "time fragment not found: " ++ toString s

class HasFragmentList a where
    getFragmentList :: a -> FragmentList

instance HasFragmentList FragmentList where
    getFragmentList = id

instance HasFragmentList Puff where
    getFragmentList (Puff {fragments = fl}) = fl

instance HasFragmentList Key where
    getFragmentList (Key _ fl) = fl

instance HasFragmentList (Atom,Fragment) where
    getFragmentList f = [f]

instance HasFragmentList Fragment where
    getFragmentList (FragmentNest fl) = fl
    getFragmentList _ = []

-- flattenFragmentList :: HasFragmentList fl => fl -> FragmentList

----------------
-- Puff Pickling
----------------

writePuffs :: String -> [Puff] -> IO ()
writePuffs fn ps = attempt $ putFile fn ps

readPuffs :: String -> IO [Puff]
readPuffs fn = tryElse [] $ getFile fn


putFile fn a = atomicWrite fn $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    --h <- openBinaryFile fn WriteMode
    bh <- openBinIO h
    put_ bh a
    hClose h

--getFile fn = do
--    h <- openBinaryFile fn ReadMode
--    bh <- openBinIO h
--    get bh
    --hClose h
getFile :: Binary a => FilePath -> IO a
getFile fn = Control.Exception.bracket (openBinaryFile fn ReadMode) hClose $ \bh -> do
	hSetBuffering bh (BlockBuffering Nothing)
	bh <- openBinIO bh
	get bh

--getFile fn = do
--    z <- readBinMem fn
--    get z

categoryHead (Category (h,_)) = h
categoryCell (Category (_,c)) = c


{-
---------------------
-- generated by DrIFT
---------------------

instance Binary Puff where
    put bh (Puff aa ab ac) = do
	    pos <- putBits bh 1 0
	    put bh aa
	    put bh ab
	    put bh ac
	    return pos
    get bh = do
	    h <- getBits bh 1
	    case h of
	      0 -> do
		    aa <- get bh
		    ab <- get bh
		    ac <- get bh
		    return (Puff aa ab ac)
    sizeOf (Puff aa ab ac) = 1 + sizeOf aa + sizeOf ab + sizeOf ac

instance Binary Fragment where
    put bh (FragmentText aa) = do
	    pos <- putBits bh 3 0
	    put bh aa
	    return pos
    put bh (FragmentData ab) = do
	    pos <- putBits bh 3 1
	    put bh ab
	    return pos
    put bh (FragmentTime ac) = do
	    pos <- putBits bh 3 2
	    put bh ac
	    return pos
    put bh (FragmentInt ad) = do
	    pos <- putBits bh 3 3
	    put bh ad
	    return pos
    put bh (FragmentNest ae) = do
	    pos <- putBits bh 3 4
	    put bh ae
	    return pos
    get bh = do
	    h <- getBits bh 3
	    case h of
	      0 -> do
		    aa <- get bh
		    return (FragmentText aa)
	      1 -> do
		    ab <- get bh
		    return (FragmentData ab)
	      2 -> do
		    ac <- get bh
		    return (FragmentTime ac)
	      3 -> do
		    ad <- get bh
		    return (FragmentInt ad)
	      4 -> do
		    ae <- get bh
		    return (FragmentNest ae)
    sizeOf (FragmentText aa) = 3 + sizeOf aa
    sizeOf (FragmentData ab) = 3 + sizeOf ab
    sizeOf (FragmentTime ac) = 3 + sizeOf ac
    sizeOf (FragmentInt ad) = 3 + sizeOf ad
    sizeOf (FragmentNest ae) = 3 + sizeOf ae

instance Binary Signature where
    put bh (Unverifyable aa) = do
	    pos <- putBits bh 2 0
	    put bh aa
	    return pos
    put bh (Signed ab) = do
	    pos <- putBits bh 2 1
	    put bh ab
	    return pos
    put bh (Encrypted ac) = do
	    pos <- putBits bh 2 2
	    put bh ac
	    return pos
    get bh = do
	    h <- getBits bh 2
	    case h of
	      0 -> do
		    aa <- get bh
		    return (Unverifyable aa)
	      1 -> do
		    ab <- get bh
		    return (Signed ab)
	      2 -> do
		    ac <- get bh
		    return (Encrypted ac)
    sizeOf (Unverifyable aa) = 2 + sizeOf aa
    sizeOf (Signed ab) = 2 + sizeOf ab
    sizeOf (Encrypted ac) = 2 + sizeOf ac

instance Binary Key where
    put bh (Key aa ab) = do
	    pos <- putBits bh 0 0
	    put bh aa
	    put bh ab
	    return pos
    get bh = do
	    h <- getBits bh 0
	    case h of
	      0 -> do
		    aa <- get bh
		    ab <- get bh
		    return (Key aa ab)
    sizeOf (Key aa ab) = 0 + sizeOf aa + sizeOf ab

--  Imported from other files :-

-}




{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Binary Puff where
    put_ bh (Puff aa ab ac) = do
	    put_ bh aa
	    put_ bh ab
	    put_ bh ac
    get bh = do
    aa <- get bh
    ab <- get bh
    ac <- get bh
    return (Puff aa ab ac)

instance Binary Fragment where
    put_ bh (FragmentText aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (FragmentData ab) = do
	    putByte bh 1
	    put_ bh ab
    put_ bh (FragmentTime ac) = do
	    putByte bh 2
	    put_ bh ac
    put_ bh (FragmentInt ad) = do
	    putByte bh 3
	    put_ bh ad
    put_ bh (FragmentNest ae) = do
	    putByte bh 4
	    put_ bh ae
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    aa <- get bh
		    return (FragmentText aa)
	      1 -> do
		    ab <- get bh
		    return (FragmentData ab)
	      2 -> do
		    ac <- get bh
		    return (FragmentTime ac)
	      3 -> do
		    ad <- get bh
		    return (FragmentInt ad)
	      4 -> do
		    ae <- get bh
		    return (FragmentNest ae)

instance Binary Signature where
    put_ bh (Unverifyable aa) = do
	    putByte bh 0
	    put_ bh aa
    put_ bh (Signed ab) = do
	    putByte bh 1
	    put_ bh ab
    put_ bh (Encrypted ac) = do
	    putByte bh 2
	    put_ bh ac
    get bh = do
	    h <- getByte bh
	    case h of
	      0 -> do
		    aa <- get bh
		    return (Unverifyable aa)
	      1 -> do
		    ab <- get bh
		    return (Signed ab)
	      2 -> do
		    ac <- get bh
		    return (Encrypted ac)

instance Binary Key where
    put_ bh (Key aa ab) = do
	    put_ bh aa
	    put_ bh ab
    get bh = do
    aa <- get bh
    ab <- get bh
    return (Key aa ab)

--  Imported from other files :-
