-- arch-tag: 0863bbd6-b533-440d-b88f-77871b5359be
module Gale.KeyCache(
    dumpKey,
    KeyCache,
    newKeyCache,
    getKey,
    getPKey,
    parseKey,
    keyIsPubKey,
    keyIsPrivKey,
    keyIsPublic,
    putKey,
    noKey,
    numberKeys,
    keyRequestPuff

    ) where

import Atom
import Bits
import Char
import Control.Concurrent
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Directory
import EIO
import ErrorLog
import Gale.Proto
import GenUtil
import List
import Maybe
import Monad
import PackedString
import Gale.Puff
import RSA
import SimpleParser
import System.Mem.Weak
import Word
import qualified Data.Map as Map

stons :: [Char] -> [Word8]
stons = map (fromIntegral . ord)

private_magic1, private_magic2, private_magic3 :: [Word8]
pubkey_magic1, pubkey_magic2 :: [Word8]

private_magic1 = stons "h\DC3\000\001"
private_magic2 = stons "h\DC3\000\003"
private_magic3 = stons "GALE\000\002"


pubkey_magic1 = stons "h\DC3\000\000"
pubkey_magic2 = stons "h\DC3\000\002"

galeRSAModulusBits = 1024
galeRSAModulusLen = (galeRSAModulusBits + 7) `div` 8
galeRSAPrimeBits = (galeRSAModulusBits + 1) `div` 2
galeRSAPrimeLen = (galeRSAPrimeBits + 7) `div` 8


data KeyCache = KeyCache {
    pkeyCache :: !(MVar (Map.Map String (Weak (Key,EvpPkey)))),
    kkeyCache :: !(MVar (Map.Map String (Weak Key))),
    galeDir :: String
    }

numberKeys kc = fmap Map.size $ readMVar (kkeyCache kc)

keyIsPubKey k =  (hasFragment k f_rsaExponent)
keyIsPrivKey k =  (hasFragment k f_rsaPrivateExponent)
keyIsPublic k = any nullPS $ getFragmentStrings k f_keyMember

newKeyCache galeDir = do
    pk <- newMVar Map.empty
    kk <- newMVar Map.empty
    return KeyCache { {- keyCache = kc, publicKeyCache = pkc,-} galeDir = galeDir, pkeyCache = pk, kkeyCache = kk }


keyToRSAElems :: Monad m => Key -> m (RSAElems [Word8])
keyToRSAElems fl = do
    if not (keyIsPubKey fl) then fail "key does not have bits" else do
    n <- getFragmentData fl f_rsaModulus
    e <- getFragmentData fl f_rsaExponent
    if not (keyIsPrivKey fl) then
        return RSAElemsPublic { rsaN = elems n, rsaE = elems e } else do
    d <- getFragmentData fl f_rsaPrivateExponent
    iqmp <- getFragmentData fl f_rsaPrivateCoefficient
    pq <- getFragmentData fl f_rsaPrivatePrime
    dmpq1 <- getFragmentData fl f_rsaPrivatePrimeExponent
    let (p,q) = splitAt galeRSAPrimeLen (elems pq)     -- should be "rsa.bits"?
        (dmp1,dmq1) = splitAt galeRSAPrimeLen (elems dmpq1)
    return RSAElemsPrivate {
        rsaN = elems n,
        rsaE = elems e,
        rsaD = elems d ,
        rsaIQMP = elems iqmp,
        rsaP = p,
        rsaQ =  q,
        rsaDMP1 = dmp1,
        rsaDMQ1 = dmq1
        }



keyToPkey :: Key -> IO EvpPkey
keyToPkey key  = do
    re <- keyToRSAElems key
    createPkey re

getPKey :: KeyCache -> String -> IO (Maybe (Key,EvpPkey))
getPKey kc kn =  modifyMVar (pkeyCache kc) f where
    f pkeyCache | Just v <- Map.lookup kn pkeyCache = do
        v <- deRefWeak v
        case v of
            Just _ -> return (pkeyCache, v)
            Nothing -> g pkeyCache
    f pkeyCache = g pkeyCache
    g pkeyCache = do
        k <- getKey kc kn
        case k of
            Nothing -> return (pkeyCache, Nothing)
            Just v | not (hasFragment v f_rsaModulus) ->  return (pkeyCache, Nothing)
            Just key -> do
                --rsa <- keyToRSA v
                --pkey <- pkeyNewRSA rsa
                pkey <- keyToPkey key
                let kp =  (key,pkey)
                ptr <- mkWeakPtr kp Nothing
                return (Map.insert  kn ptr pkeyCache, Just kp)

blankKey kn = (Key kn [])

noKey :: KeyCache -> String -> IO ()
noKey kc kn = modifyMVar (kkeyCache kc) f where
    f kkeyCache | Just _ <- Map.lookup kn kkeyCache = return (kkeyCache, ())
    f kkeyCache = do
        n <- mkWeakPtr (blankKey kn) Nothing
        return (Map.insert kn n  kkeyCache, ())




putKey :: KeyCache -> (UArray Int Word8) -> IO ()
putKey kc xs = do
    mk <- first [fmap Just (parseKey $ elems xs),return Nothing]
    case mk of
        Nothing -> return ()
        Just v'@(Key kn _) -> do
            modifyMVar (kkeyCache kc) f where
                f kkeyCache | Just _ <- Map.lookup kn kkeyCache = return (kkeyCache, ())
                f kkeyCache = do
                    first [createDirectory $ galeDir kc ++ "/auth/", return ()]
                    first [createDirectory $ galeDir kc ++ "/auth/cache/", return ()]
                    xs <- unsafeThaw xs
                    bnds <- getBounds xs
                    atomicWrite  (galeDir kc ++ "/auth/cache/" ++ kn ++ ".gpub")  $
                        \h -> hPutArray h xs (rangeSize bnds)
                    v' <- mkWeakPtr v' Nothing
                    return (Map.insert kn v' kkeyCache, ())


getKey :: KeyCache -> String -> IO (Maybe Key)
getKey kc kn = modifyMVar (kkeyCache kc) f where
    f kkeyCache | Just v <- Map.lookup kn kkeyCache = do
        v <- deRefWeak v
        case v of
            Just _ -> return (kkeyCache, v)
            Nothing -> g kkeyCache
    f kkeyCache = g kkeyCache
    g kkeyCache = do
	    v <- ioM getFromDisk
            case v of
                Nothing -> return (kkeyCache, Nothing)
                Just v' -> do
                    v'' <- mkWeakPtr v' Nothing
                    return (Map.insert kn v'' kkeyCache, Just v')
    getFromDisk :: IO Key
    getFromDisk = do
        let pc = galeDir kc ++ "/auth/cache/"
            pp = galeDir kc ++ "/auth/private/"
            knames = [ pp ++ kn ++ ".gpri", pp ++ kn ++ ".gpub", pp ++ kn , pc ++ kn ++ ".gpub" ]
            gn = (map (\fn -> (readRawFile fn >>= parseKey >>= \c -> return (c,fn))) knames)
        --putLog LogDebug $ "Looking for: " ++ show  knames
        xs <- mapM tryMost gn
        let ks = [ k | Right (k@(Key n _),_) <- xs, kn == n]
        let nk = foldr (\(Key _ fl) (Key n fl') -> Key n (fl `mergeFrags` fl')) (Key kn []) ks
        --(key_c, fn) <- (first  gn)
	--key <- parseKey key_c
	--rsa <- pubKeyToRSA key
	--pkey <- pkeyNewRSA rsa
        if null (getFragmentList nk) then ioError $ userError "bad key" else return nk

flipLocalPart :: String -> String
flipLocalPart s | '@' `notElem` s = s
flipLocalPart s = nbp ++ ep where
    nbp = concat $ reverse (groupBy f bp)
    (bp,ep) = span (/= '@') s
    f '.' '.' = True
    f x y | x /= '.' && y /= '.' = True
    f _ _ = False

keyRequestPuff :: String -> Puff
keyRequestPuff s = emptyPuff { cats = [Category ("_gale.query." ++ n, d)], fragments = [(f_questionKey, FragmentText $ packString s), (f_questionKey',FragmentText $ packString s')]} where
    s' = flipLocalPart s
    Category (n,d) = catParseNew s

la xs = listArray (0, length xs - 1) xs

keyDecode12 xs = [ (fromString x,y) | (x,y) <- fl] where
    fl = [("rsa.modulus",FragmentData $ la modulusD),
	  ("rsa.exponent",FragmentData $ la exponentD),
	  ("rsa.private.exponent",FragmentData $ la privateExponentD),
	  ("rsa.private.prime",FragmentData $ la privatePrimeD),
	  ("rsa.private.prime.exponent",FragmentData $ la privatePrimeExponentD),
	  ("rsa.private.coefficient",FragmentData $ la privateCoefficientD),
	  ("rsa.bits", FragmentInt (fromIntegral bits))
	 ]
    (bits,ys) = xdrReadUInt xs
    (modulusD,xs') = splitRLE galeRSAModulusLen ys
    (exponentD,xs'') = splitRLE galeRSAModulusLen xs'
    (privateExponentD,xs''') = splitRLE (galeRSAPrimeLen * 2) xs''
    (privatePrimeD,xs'''') = splitRLE galeRSAModulusLen xs'''
    (privatePrimeExponentD,xs''''') = splitRLE (galeRSAPrimeLen * 2) xs''''
    (privateCoefficientD,_) = splitRLE galeRSAPrimeLen xs'''''



keyParse :: GenParser Word8 Key
keyParse = choice [ppk1,ppk2,ppk3,pk1,pk2,pk3] where
    ppk1 = do
	parseExact private_magic1
	kn <- fmap flipLocalPart parseNullString
	fl <- toParser keyDecode12
	return $ Key kn fl
    ppk2 = do
	parseExact private_magic2
	kn <- fmap flipLocalPart parseLenString
	fl <- toParser keyDecode12
	return $ Key kn fl
    ppk3 = do
	parseExact private_magic3
	kn <- fmap flipLocalPart parseLenString
	fl <- toParser decodeFragments
	return $ Key kn fl
    pk1 = do
	parseExact pubkey_magic1
	kn <- fmap flipLocalPart parseNullString
	(eof >> return (Key kn [])) <|> do
	comment <- parseNullString
	fl <- parsePublic12
	_signature <- parseRest
	return $ Key kn ([(f_keyOwner, FragmentText (packString comment))] ++ fl)
    pk2 = do
	parseExact pubkey_magic2
	kn <- fmap flipLocalPart parseLenString
	(eof >> return (Key kn [])) <|> do
	comment <- parseLenString
	fl' <- parsePublic12
	let fl = ((f_keyOwner, FragmentText (packString comment)):fl')
	(eof >> return (Key kn fl)) <|> do
	ts <- parseSome 16
	te <- parseSome 16
	_signature <- parseRest
	return $ Key kn $ fl ++ [(f_keySigned,FragmentTime (decodeTime ts)), (f_keyExpires,FragmentTime (decodeTime te))]
    pk3 = do
	parseExact pubkey_magic3
	kn <- fmap flipLocalPart parseLenString
	fl <- toParser decodeFragments
	return $ Key kn fl
    parsePublic12 = do
	bits <- parseIntegral32
	modulus <- (MkP (\x -> Just (splitRLE galeRSAModulusLen x)))
	exponent <- (MkP (\x -> Just (splitRLE galeRSAModulusLen x)))
	return [(f_rsaModulus, FragmentData $ la modulus),
		(f_rsaExponent, FragmentData $ la exponent),
		(f_rsaBits, FragmentInt bits)]


parseKey :: [Word8] -> IO Key
parseKey xs = do
    (Key kn fl) <- parser keyParse xs
    fl <- unsignFragments fl
    return $ Key kn fl

splitRLE :: Int -> [Word8] -> ([Word8], [Word8])
splitRLE n _ | n < 0 = error "invalid RLE encoding"
splitRLE 0 xs = ([],xs)
splitRLE n (c:xs) | c .&. 0x80 /= 0 = (take count xs ++ ys,rest) where
    count = fromIntegral $ (c .&. 0x7f) + 1
    (ys,rest) = splitRLE (n - count) (drop count xs)
splitRLE n (c:x:xs) | c .&. 0x80 == 0 = (replicate count x ++ ys,rest) where
    count = fromIntegral $ (c .&. 0x7f) + 1
    (ys,rest) = splitRLE (n - count) xs
splitRLE _ _  = error "invalid RLE encoding"

toParser :: ([c] -> a) -> GenParser c a
toParser f = MkP (\xs -> Just (f xs, []))

unsignData :: [Word8] -> IO (FragmentList,Key)
unsignData xs = do
    key <- parseKey $ take (fromIntegral (l - (8 + sb))) (drop (fromIntegral sb) xs'')
    return (fl,key) where
	(l,xs') = xdrReadUInt xs
	(sb,xs'') = xdrReadUInt (drop 4 xs')
	fl = (decodeFragments $ drop (fromIntegral l + 4) xs')

unsignFragments :: FragmentList -> IO FragmentList
unsignFragments tfl | (xs:_) <- [xs | (n,FragmentData xs) <- tfl, n == f_securitySignature] = do
    (fl,_) <- unsignData (elems xs)
    unsignFragments fl
unsignFragments x = return x

----------------
-- test code
----------------

{-# NOTINLINE dumpKey #-}
dumpKey :: String -> IO ()
dumpKey arg = do
    gd <- getGaleDir
    kc <- newKeyCache gd
    nk <- getKey kc arg
    key <- case nk of
        Just key -> return key
        Nothing -> do
            c <- readRawFile arg
            parseKey c
    putStrLn $ showKey key



{-

getPrivateKey :: KeyCache -> String -> IO (Maybe (Key,EvpPkey))
getPrivateKey gc kn = modifyMVar (keyCache gc) f where
    f kc = case [x|x@(Key kn' _,_) <- kc, kn == kn'] of
	(v:_) -> return (kc,Just v)
	[] -> do
	    v <- ioMp getFromDisk
	    return ((maybeToList v ++ kc),v)
    getFromDisk = do
	let gd = galeDir gc
	let p = (gd ++ "/auth/private/")
	    knames = [p ++ kn, p ++ kn ++ ".gpri"]
	    gn = (map (\fn -> (readRawFile fn >>= \c -> return (c,fn))) knames)
	(key_c, fn) <- (first  gn)
	key <- parseKey key_c
	rsa <- privkeyToRSA key
	pkey <- pkeyNewRSA rsa
	putLog LogNotice $ "Retrieved private key from disk: " ++ fn
	return (key,pkey)



getPublicKey :: KeyCache -> String -> IO (Maybe (Key,EvpPkey))
getPublicKey gc kn = modifyMVar (publicKeyCache gc) f where
    f keyCache | Just v <- lookupFM keyCache kn = return (keyCache, Just v)
    f keyCache = do
	    v <- ioM getFromDisk
            return $ case v of
                Nothing -> (keyCache, Nothing)
                Just v' -> (addToFM keyCache kn v', Just v')
    getFromDisk :: IO (Key,EvpPkey)
    getFromDisk = do
        let gd = galeDir gc
        let p = (gd ++ "/auth/cache/")
            knames = [ p ++ kn ++ ".gpub"]
            gn = (map (\fn -> (readRawFile fn >>= \c -> return (c,fn))) knames)
        putLog LogDebug $ "Looking for: " ++ show  knames
        (key_c, fn) <- (first  gn)
	key <- parseKey key_c
	rsa <- pubKeyToRSA key
	pkey <- pkeyNewRSA rsa
        return (key,pkey)
-}

{-
fragmentData' x y = fragmentData (packString x) y
privkeyToRSA :: Key -> IO RSA
privkeyToRSA (Key _ fl)  = do
    rsa <- rsaNew
    fragmentData' "rsa.modulus" fl >>= bn_bin2bn >>= rsaSetN rsa
    fragmentData' "rsa.exponent" fl >>= bn_bin2bn >>= rsaSetE rsa
    fragmentData' "rsa.private.exponent" fl >>= bn_bin2bn >>= rsaSetD rsa
    fragmentData' "rsa.private.coefficient" fl >>= bn_bin2bn >>= rsaSetIQMP rsa
    xs <- fragmentData' "rsa.private.prime" fl
    let (p,q) = splitAt galeRSAPrimeLen xs
    bn_bin2bn p >>= rsaSetP rsa
    bn_bin2bn q >>= rsaSetQ rsa
    xs <- fragmentData' "rsa.private.prime.exponent" fl
    let (dmp1,dmq1) = splitAt galeRSAPrimeLen xs
    bn_bin2bn dmp1 >>= rsaSetDMP1 rsa
    bn_bin2bn dmq1 >>= rsaSetDMQ1 rsa
    rsaCheckKey rsa
    return rsa

pubKeyToRSA :: Key -> IO RSA
pubKeyToRSA (Key _ fl)  = do
    rsa <- rsaNew
    fragmentData' "rsa.modulus" fl >>= bn_bin2bn >>= rsaSetN rsa
    fragmentData' "rsa.exponent" fl >>= bn_bin2bn >>= rsaSetE rsa
    --fragmentData' "rsa.private.exponent" fl >>= bn_bin2bn >>= rsaSetD rsa
    --fragmentData' "rsa.private.coefficient" fl >>= bn_bin2bn >>= rsaSetIQMP rsa
    --xs <- fragmentData' "rsa.private.prime" fl
    --let (p,q) = splitAt galeRSAPrimeLen xs
    --bn_bin2bn p >>= rsaSetP rsa
    --bn_bin2bn q >>= rsaSetQ rsa
    --xs <- fragmentData' "rsa.private.prime.exponent" fl
    --let (dmp1,dmq1) = splitAt galeRSAPrimeLen xs
    --bn_bin2bn dmp1 >>= rsaSetDMP1 rsa
    --bn_bin2bn dmq1 >>= rsaSetDMQ1 rsa
    --rsaCheckKey rsa
    return rsa

-}
{-
keyToRSA :: Key -> IO RSA
keyToRSA (Key _ fl)  = do
    rsa <- rsaNew
    fragmentData' "rsa.modulus" fl >>= bn_bin2bn >>= rsaSetN rsa
    fragmentData' "rsa.exponent" fl >>= bn_bin2bn >>= rsaSetE rsa
    if  (hasFragment fl (packString "rsa.private.exponent")) then  do
        fragmentData' "rsa.private.exponent" fl >>= bn_bin2bn >>= rsaSetD rsa
        fragmentData' "rsa.private.coefficient" fl >>= bn_bin2bn >>= rsaSetIQMP rsa
        xs <- fragmentData' "rsa.private.prime" fl
        let (p,q) = splitAt galeRSAPrimeLen xs     -- should be "rsa.bits"?
        bn_bin2bn p >>= rsaSetP rsa
        bn_bin2bn q >>= rsaSetQ rsa
        xs <- fragmentData' "rsa.private.prime.exponent" fl
        let (dmp1,dmq1) = splitAt galeRSAPrimeLen xs
        bn_bin2bn dmp1 >>= rsaSetDMP1 rsa
        bn_bin2bn dmq1 >>= rsaSetDMQ1 rsa
        rsaCheckKey rsa
      else do
        return ()

       -- rsaSetD rsa nullPtr
       -- rsaSetIQMP rsa nullPtr
       -- rsaSetP rsa nullPtr
       -- rsaSetQ rsa nullPtr
       -- rsaSetDMP1 rsa nullPtr
       -- rsaSetDMQ1 rsa nullPtr
    return rsa
-}
