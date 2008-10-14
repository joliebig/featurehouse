-- arch-tag: f5954078-d1ae-430d-8bf5-e89bef6beeb3
module RSA(
    EvpPkey,
    decryptAll,
    encryptAll,
    signAll,
    createPkey,
    RSAElems(..)
    ) where


#include "my_rsa.h"

import CForeign
import Control.Exception as E
import ErrorLog
import Foreign
import Foreign.ForeignPtr
import MarshalArray
import System.IO.Unsafe

data RSAStruct
data EVP_PKEY
data EVP_CIPHER
data EVP_CIPHER_CTX
data EVP_MD_CTX
data EVP_MD
data BIGNUM

type RSA = Ptr RSAStruct

type EvpPkey = ForeignPtr EVP_PKEY

fi x = fromIntegral x

------------------------
-- marshalling utilities
------------------------

throwZero_ s = throwIf_ (== 0) (const s)

withData :: [Word8] -> (Ptr CUChar -> CInt -> IO a) -> IO a
withData xs f = withArray xs (\a -> f (castPtr a) (fromIntegral $ length xs))

returnData :: Int -> (Ptr CUChar -> Ptr CInt -> IO z) -> IO [Word8]
returnData sz f = do
	alloca (\bp -> allocaArray sz (\m -> f m bp >> peek bp >>= \s -> fmap (map fromIntegral) (peekArray (fromIntegral s) m)))

---------------
-- RSA routines
---------------


foreign import ccall unsafe "my_rsa.h RSA_new" rsaNew :: IO RSA
-- foreign import ccall unsafe "my_rsa.h RSA_free" rsaFree :: RSA -> IO ()

foreign import ccall unsafe "my_rsa.h RSA_check_key" rsa_check_key :: RSA -> IO CInt

rsaCheckKey :: RSA -> IO ()
rsaCheckKey rsa = throwZero_ "RSA_check_key" $ rsa_check_key rsa

data RSAElems a = RSAElemsPrivate { rsaN :: a, rsaE :: a, rsaD :: a, rsaIQMP :: a, rsaP :: a, rsaQ :: a, rsaDMP1 :: a, rsaDMQ1 :: a } |  RSAElemsPublic { rsaN :: a, rsaE :: a }

{-
rsaSetN :: RSA -> Ptr BIGNUM -> IO ()
rsaSetN = (#poke RSA, n)

rsaSetE :: RSA -> Ptr BIGNUM -> IO ()
rsaSetE = (#poke RSA, e)

rsaSetD :: RSA -> Ptr BIGNUM -> IO ()
rsaSetD = (#poke RSA, d)

rsaSetP :: RSA -> Ptr BIGNUM -> IO ()
rsaSetP = (#poke RSA, p)

rsaSetQ :: RSA -> Ptr BIGNUM -> IO ()
rsaSetQ = (#poke RSA, q)

rsaSetDMP1 :: RSA -> Ptr BIGNUM -> IO ()
rsaSetDMP1 = (#poke RSA, dmp1)

rsaSetDMQ1 :: RSA -> Ptr BIGNUM -> IO ()
rsaSetDMQ1 = (#poke RSA, dmq1)

rsaSetIQMP :: RSA -> Ptr BIGNUM -> IO ()
rsaSetIQMP = (#poke RSA, iqmp)

-}



----------------------
-- Envelope Encryption
----------------------

foreign import ccall unsafe "openssl/evp.h EVP_DigestInit" evpSignInit :: Ptr EVP_MD_CTX -> Ptr EVP_MD -> IO ()
foreign import ccall unsafe "openssl/evp.h EVP_DigestUpdate" evpSignUpdate :: Ptr EVP_MD_CTX -> Ptr CUChar -> CInt -> IO ()
foreign import ccall unsafe "openssl/evp.h EVP_SignFinal" evpSignFinal :: Ptr EVP_MD_CTX -> Ptr CUChar -> Ptr CInt -> Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe "openssl/evp.h EVP_OpenInit" evpOpenInit :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> Ptr CUChar -> CInt -> Ptr CUChar -> Ptr EVP_PKEY -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_DecryptUpdate" evpOpenUpdate :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_OpenFinal" evpOpenFinal :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> IO CInt

--foreign import ccall unsafe "openssl/evp.h EVP_EncryptInit" evpEncryptInit :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> Ptr CUChar -> Ptr CUChar -> IO CInt
--foreign import ccall unsafe "openssl/evp.h EVP_EncryptUpdate" evpEncryptUpdate :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> Ptr CUChar -> CInt -> IO CInt
--foreign import ccall unsafe "openssl/evp.h EVP_EncryptFinal" evpEncryptFinal :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt ->  IO CInt

foreign import ccall unsafe "openssl/evp.h EVP_SealInit" evpSealInit :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> Ptr (Ptr CUChar) -> Ptr CInt -> Ptr CUChar -> Ptr (Ptr EVP_PKEY) -> CInt -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_EncryptUpdate" evpSealUpdate :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> Ptr CUChar -> CInt -> IO CInt
foreign import ccall unsafe "openssl/evp.h EVP_SealFinal" evpSealFinal :: Ptr EVP_CIPHER_CTX -> Ptr CUChar -> Ptr CInt -> IO CInt

foreign import ccall unsafe "openssl/evp.h EVP_des_ede3_cbc" evpDesEde3Cbc :: IO (Ptr EVP_CIPHER)
foreign import ccall unsafe "openssl/evp.h EVP_md5" evpMD5 :: IO (Ptr EVP_MD)


foreign import ccall unsafe evpCipherContextBlockSize :: Ptr EVP_CIPHER_CTX -> IO Int
#def inline HsInt evpCipherContextBlockSize(EVP_CIPHER_CTX *e) {return EVP_CIPHER_block_size(EVP_CIPHER_CTX_cipher(e));}


{-
evp_OpenInit :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> [Word8] -> [Word8] -> Ptr EVP_PKEY -> IO ()
evp_OpenInit cctx cipher ek iv pkey =  do
	throwZero_ "EVP_OpenInit" $ withData ek (\a b -> withArray iv (\iv -> evpOpenInit cctx cipher a b (castPtr iv) pkey))
	return ()
-}	
evp_OpenUpdate :: Ptr EVP_CIPHER_CTX -> [Word8] -> IO [Word8]
evp_OpenUpdate cctx ind = do
    bsz <- evpCipherContextBlockSize cctx
    let sz = length ind + bsz
    withData ind (\ina inl -> returnData sz (\outa outl -> throwZero_ "EVP_OpenUpdate" (evpOpenUpdate cctx outa outl ina inl)))

evp_OpenFinal ::  Ptr EVP_CIPHER_CTX -> IO [Word8]
evp_OpenFinal cctx = do
    bsz <- evpCipherContextBlockSize cctx
    d <- returnData bsz (\outa outl -> throwZero_ "EVP_OpenFinal" (evpOpenFinal cctx outa outl))
    return d



--foreign import ccall unsafe "EVP_PKEY_new" evpPKEYNew :: IO (Ptr EVP_PKEY)
--foreign import ccall unsafe "EVP_PKEY_assign" evpPKEYAssign :: Ptr EVP_PKEY -> CInt -> Ptr a -> IO CInt
--foreign import ccall unsafe "EVP_PKEY_free" evpPKEYFree :: Ptr EVP_PKEY -> IO ()
foreign import ccall unsafe "EVP_PKEY_size" evpPKEYSize :: Ptr EVP_PKEY -> IO CInt

foreign import ccall unsafe "EVP_CIPHER_CTX_init" evpCipherCtxInit :: Ptr EVP_CIPHER_CTX -> IO ()
-- some implementations have this return int, but not all so we must ignore the return value.
foreign import ccall unsafe "EVP_CIPHER_CTX_cleanup" evpCipherCtxCleanup :: Ptr EVP_CIPHER_CTX -> IO ()

withCipherCtx :: (Ptr EVP_CIPHER_CTX -> IO a) -> IO a
withCipherCtx action = allocaBytes (#const sizeof(EVP_CIPHER_CTX)) $ \cctx ->
	    E.bracket_ (evpCipherCtxInit cctx)
		(evpCipherCtxCleanup cctx)
		    (action cctx)

withMdCtx :: (Ptr EVP_MD_CTX -> IO a) -> IO a
withMdCtx = allocaBytes (#const sizeof(EVP_MD_CTX))


decryptAll :: [Word8] -> [Word8] -> EvpPkey -> [Word8] -> IO [Word8]
decryptAll keydata iv pkey xs = withCipherCtx $ \cctx -> do
    withData keydata $ \a b -> withArray iv $ \iv -> do
    withForeignPtr pkey $ \pkey -> do
    throwZero_ "EVP_OpenInit" $ evpOpenInit cctx cipher a b (castPtr iv) pkey
    d <- evp_OpenUpdate cctx xs
    dr <- evp_OpenFinal cctx
    return $ d ++ dr

cipher = unsafePerformIO evpDesEde3Cbc
md5 = unsafePerformIO evpMD5


signAll :: EvpPkey -> [Word8] -> IO [Word8]
signAll  pkey xs = withMdCtx $ \cctx -> do
    evpSignInit cctx md5
    withData xs $ \a b -> (evpSignUpdate cctx a b)
    withForeignPtr pkey $ \pkey -> do
    bsz <- fmap fromIntegral $ evpPKEYSize pkey
    d <- returnData bsz (\outa outl -> throwZero_ "EVP_SignFinal" (evpSignFinal cctx outa outl pkey))
    return d

--putLog _ = putStrLn


withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs ps action = fp' ps []   where
    fp' [] xs = action (reverse xs)
    fp' (p:ps) xs = withForeignPtr p (\x -> fp' ps (x:xs))

encryptAll :: [EvpPkey] -> [Word8] -> IO ([Word8], [[Word8]], [Word8])
encryptAll [] _ = error "encryptAll: no keys"
encryptAll  keys xs = doit' where
    doit' = do
            putLog LogDebug $ "encryptAll: " ++ show keys
            doit
    doit = do
        withCipherCtx $ \cctx -> do
        allocaArray n $ \ek -> do
        allocaArray n $ \ekl -> do
        allocaBytes ivs $ \iv -> do
        withForeignPtrs keys $ \keys -> do
        withArray keys $ \pubk -> do
            putLog LogDebug $ "encryptAll: " ++ show keys
            mapM_ (\n -> peekElemOff ek n >>= \v -> putLog LogDebug $ "ea: " ++ show (n,v)) [0 .. n - 1]
            bsz <- fmap (fi . maximum) $ mapM  ( evpPKEYSize) (keys)
            putLog LogDebug $ "encryptAll: bsz " ++ show bsz
            foldr (\n r -> allocaBytes bsz (\x -> pokeElemOff ek n x >> r)) (rest cctx ek ekl iv pubk) [0 .. n - 1]
    rest cctx ek ekl iv pubk = do
        mapM_ (\n -> peekElemOff ek n >>= \v -> putLog LogDebug $ "ea: " ++ show (n,v)) [0 .. n - 1]
        ---withForeignPtr pubk $ \pubk -> do
        throwZero_ "EVPSealInit" $ evpSealInit cctx cipher ek ekl iv pubk (fi n)
        bsize <- evpCipherContextBlockSize cctx
        putLog LogDebug $ "encryptAll: bsize " ++ show bsize
        rd <- returnData (dsize + bsize) $ \ra rb ->  withData xs $ \a b -> (throwZero_ "EVP_SealUpdate" $ evpSealUpdate cctx ra rb a b)
        d <- returnData bsize (\outa outl -> throwZero_ "EVP_SealFinal" (evpSealFinal cctx outa outl))
        iva <- peekArray ivs (castPtr iv)
        ks <- mapM (pa ek ekl) [0 .. n - 1]
        return (rd ++ d,ks,iva)
    pa ek ekl n = do
        l <- peekElemOff ekl n
        p <- peekElemOff ek n
        peekArray (fi l) (castPtr p)
    n = length keys
    ivs = 8
    dsize = length xs
--    bsize = 64



--------------------
-- BigNum routines
--------------------

--bn_bin2bn :: [Word8] -> IO (Ptr BIGNUM)
--bn_bin2bn xs = throwIfNull "BN_bin2bn" $ withData xs (\a b -> bnBin2Bn a b nullPtr)

foreign import ccall unsafe "BN_bin2bn" bnBin2Bn :: Ptr CUChar -> CInt -> Ptr BIGNUM -> IO (Ptr BIGNUM)


type NEvpPkey = ForeignPtr EVP_PKEY

foreign import ccall unsafe pkeyNewRSA :: RSA -> IO (Ptr EVP_PKEY)
#def inline EVP_PKEY *pkeyNewRSA(RSA *rsa) {EVP_PKEY *pk; pk = EVP_PKEY_new(); EVP_PKEY_assign_RSA(pk, rsa); return pk;}

-- foreign import ccall "&EVP_PKEY_free" evpPkeyFreePtr :: FunPtr (Ptr EVP_PKEY -> IO ())
foreign import ccall "get_KEY" evpPkeyFreePtr :: FunPtr (Ptr EVP_PKEY -> IO ())
#def inline HsFunPtr  get_KEY (void) {return (HsFunPtr)&EVP_PKEY_free;}

createPkey :: RSAElems [Word8] -> IO NEvpPkey
createPkey re =  create_rsa re >>= create_pkey where
    setBn pb d = do
        np <- peek pb
        n <- withData d (\a b -> bnBin2Bn a b np)
        poke pb n
    create_private _ RSAElemsPublic {} = return ()
    create_private rsa re = do
        setBn ((#ptr RSA, d) rsa) (rsaD re)
        setBn ((#ptr RSA, iqmp) rsa) (rsaIQMP re)
        setBn ((#ptr RSA, p) rsa) (rsaP re)
        setBn ((#ptr RSA, q) rsa) (rsaQ re)
        setBn ((#ptr RSA, dmp1) rsa) (rsaDMP1 re)
        setBn ((#ptr RSA, dmq1) rsa) (rsaDMQ1 re)
        rsaCheckKey rsa
    create_rsa re = do
        let n = rsaN re
            e = rsaE re
        rsa <- rsaNew
        np <- (#peek RSA, n) rsa
        n <- withData n (\a b -> bnBin2Bn a b np)
        (#poke RSA, n) rsa n
        ep <- (#peek RSA, e) rsa
        e <- withData e (\a b -> bnBin2Bn a b ep)
        (#poke RSA, e) rsa e
        create_private rsa re
        return rsa
    create_pkey rsa = do
        pkey <- pkeyNewRSA rsa
        newForeignPtr evpPkeyFreePtr pkey


{-


readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
  allocaBytes (#const PATH_MAX) $ \buf -> do

    len <- withCString file $ \s ->
      throwErrnoIfMinus1 "readSymbolicLink" $
	c_readlink s buf (#const PATH_MAX)
    peekCStringLen (buf,fromIntegral len)

foreign import ccall unsafe "unistd.h readlink"
  c_readlink :: CString -> CString -> CInt -> IO CInt

-}
