
module Network.GnuTLS.X509 (
	-- * Types
	Certificate(),
	PrivateKey(),
	-- * Certificate Functions
	--newCertificate,
	--newPrivateKey,
	importCertificate,
	importPrivateKey,
	exportCertificate,
	exportPrivateKey,
	verifySignature,
	signData,
	certificateRSAParameters,
	privateKeyRSAParameters,
	privateKeyGetKeyId,
	certificateGetKeyId,
	getIssuerDnByOid,
	getDnByOid,
	getExtensionByOid,
--	getPeersX509DN,
	checkHostname,
	checkIssuer,
--	activationTime,
--	expirationTime,
--	getPeersCertificatesX509
	)
	where

import Foreign
import Foreign.C
import Network.GnuTLS.Attributes
import Network.GnuTLS.GnuTLS
import Network.GnuTLS.Internals
import Network.GnuTLS.GnuTLSMonad
import Network.GnuTLS.Errors
import Network.GnuTLS.OID
import System.Time(ClockTime(TOD))
import Data.ByteString.Base
import System.Time

#include <gnutls/x509.h>

{#context prefix = "gnutls_x509" #}

{#pointer gnutls_x509_crt_t as Certificate foreign newtype #}
{#pointer gnutls_x509_privkey_t as PrivateKey foreign newtype #}
{#pointer gnutls_x509_crl_t as CertificateRevocationList foreign newtype #}

instance Show PrivateKey where
	show _ = "<PrivateKey>"

instance Show Certificate where
	show _ = "<Certificate>"

#c
#ifdef COMPAT_GNUTLS_1_0
typedef gnutls_x509_crt gnutls_x509_crt_t;
typedef gnutls_x509_crl gnutls_x509_crl_t;
#endif
#endc

-- Certificate Initializaion

foreign import ccall unsafe "TLS.h &gnutls_x509_crt_deinit" gnutls_x509_crt_deinit :: FunPtr (Ptr Certificate -> IO ())
foreign import ccall unsafe "TLS.h gnutls_x509_crt_init"   gnutls_x509_crt_init :: Ptr (Ptr Certificate) -> IO CInt

foreign import ccall unsafe "TLS.h &gnutls_x509_privkey_deinit" gnutls_x509_privkey_deinit :: FunPtr (Ptr PrivateKey -> IO ())
foreign import ccall unsafe "TLS.h gnutls_x509_privkey_init" gnutls_x509_privkey_init :: Ptr (Ptr PrivateKey) -> IO CInt

-- | Import an encoded certificate to the native format.
importCertificate :: Datum d => d -> X509CertificateFormat -> GnuTLSMonad Certificate
importCertificate dat form = unsafePerformIO $ withDatum dat $
	\rdat -> alloca $ \ptr -> do
		res <- gnutls_x509_crt_init ptr
		if res/=0 then return (Left $ GnuTLSError res)
			  else do
			  	fp <- newForeignPtr gnutls_x509_crt_deinit =<< peek ptr
				res2 <- withForeignPtr fp (\cr -> {#call unsafe gnutls_x509_crt_import#} cr rdat (enumCInt form))
				if res2/=0 then return (Left $ GnuTLSError res2)
					   else return (Right $ Certificate fp)

importPrivateKey :: Datum d => d -> X509CertificateFormat -> GnuTLSMonad PrivateKey
importPrivateKey dat form = unsafePerformIO $ withDatum dat $
	\rdat -> alloca $ \ptr -> do
		res <- gnutls_x509_privkey_init ptr
		if res/=0 then return (Left $ GnuTLSError res)
			  else do
			  	fp <- newForeignPtr gnutls_x509_privkey_deinit =<< peek ptr
				res2 <- withForeignPtr fp (\cr -> {#call unsafe gnutls_x509_privkey_import#} cr rdat (enumCInt form))
				if res2/=0 then return (Left $ GnuTLSError res2)
					   else return (Right $ PrivateKey fp)

{-getPeersCertificatesX509 :: Session t -> IO [Certificate]
getPeersCertificatesX509 ses = mapM p =<< getPeersCertificatesRaw ses
    where p raw = do c <- newCertificate
                     importCertificate c raw X509FmtDer
                     return c-}

exportCertificate :: Certificate -> X509CertificateFormat -> GnuTLSMonad ByteString
exportCertificate cert form = unsafePerformIO $ exportCertificate'' cert form 1024

exportPrivateKey :: PrivateKey -> X509CertificateFormat -> GnuTLSMonad ByteString
exportPrivateKey key form = unsafePerformIO $ export'' (exportPrivateKey' key form) 1024

export'' :: (Ptr () -> Ptr CSize -> IO CInt) -> Int -> IO (GnuTLSMonad ByteString)
export'' f sz = do
	(str,res) <- createAndTrim' sz $ \ptr -> with (fromIntegral sz) $ \szptr -> do
		res <- f (castPtr ptr) szptr
		case () of
			() | res==0 -> do
				rsize <- peek szptr
				return (0,fromIntegral rsize,Nothing)
			   | res==errorShortMemory -> return (0,0,Just Nothing)
			   | otherwise -> return (0,0,Just $ Just res)
	case res of
		Nothing -> return $ Right str
		Just Nothing -> export'' f (sz+1024)
		Just (Just err) -> return $ Left (GnuTLSError err)

exportCertificate'' :: Certificate -> X509CertificateFormat -> Int -> IO (GnuTLSMonad ByteString)
exportCertificate'' cert format sz = export'' (exportCertificate' cert format) sz

-- | Helper function
exportCertificate' :: Certificate -> X509CertificateFormat -> Ptr () -> Ptr CSize -> IO CInt
exportCertificate' cert format ptr sptr = withCertificate cert $ \cp -> {#call unsafe gnutls_x509_crt_export#} cp (enumCInt format) ptr (castPtr sptr)
		
-- | Helper function
exportPrivateKey' :: PrivateKey -> X509CertificateFormat -> Ptr () -> Ptr CSize -> IO CInt
exportPrivateKey' key format ptr sptr = withPrivateKey key $ \cp -> {#call unsafe gnutls_x509_privkey_export#} cp (enumCInt format) ptr (castPtr sptr)

{#fun unsafe gnutls_x509_crt_get_expiration_time as getExpirationTime
  {withCertificate* `Certificate'} -> `ClockTime' toTime* #}

toTime :: Integral n => n -> IO ClockTime
toTime (-1) = fail "Error while getting time"
toTime x    = return $ TOD (fromIntegral x) 0

verifySignature :: (Datum dat,Datum sign) => Certificate -> dat -> sign -> GnuTLSMonad Bool
verifySignature cert dat sig = unsafePerformIO $ withCertificate cert $
	\rcert -> withDatum dat $
	\rdat -> withDatum sig $
	\rsig -> do
		res <- {#call unsafe gnutls_x509_crt_verify_data#} rcert 0 rdat rsig
		case () of
			() | res == 0 -> return $ Right False
			   | res == 1 -> return $ Right True
			   | otherwise -> return $ Left $ GnuTLSError res

signData :: Datum d => PrivateKey -> DigestAlgorithm -> d -> GnuTLSMonad ByteString
signData key alg dat = unsafePerformIO $ signData'' key alg dat 1024

signData'' :: Datum d => PrivateKey -> DigestAlgorithm -> d -> Int -> IO (GnuTLSMonad ByteString)
signData'' key alg dat sz = do
	(str,res) <- createAndTrim' sz $ \ptr -> with (fromIntegral sz) $ \szptr -> do
		res <- signData' key alg dat (castPtr ptr) szptr
		case () of
			() | res==0 -> do
				rsize <- peek szptr
				return (0,fromIntegral rsize,Nothing)
			   | res==errorShortMemory -> return (0,0,Just Nothing)
			   | otherwise -> return (0,0,Just $ Just res)
	case res of
		Nothing -> return $ Right str
		Just Nothing -> signData'' key alg dat (sz+1024)
		Just (Just err) -> return $ Left (GnuTLSError err)

signData' :: Datum d => PrivateKey -> DigestAlgorithm -> d -> Ptr () -> Ptr CSize -> IO CInt
signData' priv alg dat strptr sptr = withPrivateKey priv $
	\rpriv -> withDatum dat $
	\rdat -> {#call unsafe gnutls_x509_privkey_sign_data#} rpriv (enumCInt alg) 0 rdat strptr (castPtr sptr)


certificateRSAParameters :: Certificate -> GnuTLSMonad (ByteString,ByteString)
certificateRSAParameters cert = unsafePerformIO $ withCertificate cert $
	\rcert -> alloca $ \(ptrM::Ptr (Ptr ())) -> alloca $
	\(ptrE::Ptr (Ptr ())) -> do
		res <- {#call unsafe gnutls_x509_crt_get_pk_rsa_raw#} rcert (castPtr ptrM) (castPtr ptrE)
		if res/=0 then return $ Left (GnuTLSError res)
			  else do
			  	(cptrM,sizeM) <- peekDatum ptrM
				(cptrE,sizeE) <- peekDatum ptrE
				fpM <- newForeignPtr c_free_finalizer (castPtr cptrM)
				fpE <- newForeignPtr c_free_finalizer (castPtr cptrE)
				return $ Right (fromForeignPtr fpM sizeM,fromForeignPtr fpE sizeE)

privateKeyRSAParameters :: PrivateKey -> GnuTLSMonad (ByteString,ByteString,ByteString,ByteString,ByteString,ByteString)
privateKeyRSAParameters key = unsafePerformIO $ withPrivateKey key $
	\rkey -> alloca $
	\(ptrM::Ptr (Ptr ())) -> alloca $
	\(ptrE::Ptr (Ptr ())) -> alloca $
	\(ptrD::Ptr (Ptr ())) -> alloca $
	\(ptrP::Ptr (Ptr ())) -> alloca $
	\(ptrQ::Ptr (Ptr ())) -> alloca $
	\(ptrU::Ptr (Ptr ())) -> do
		res <- {#call unsafe gnutls_x509_privkey_export_rsa_raw#} rkey
			(castPtr ptrM)
			(castPtr ptrE)
			(castPtr ptrD)
			(castPtr ptrP)
			(castPtr ptrQ)
			(castPtr ptrU)
		if res/= 0 then return $ Left (GnuTLSError res)
			   else do
			   	(cptrM,sizeM) <- peekDatum ptrM
			   	(cptrE,sizeE) <- peekDatum ptrE
			   	(cptrD,sizeD) <- peekDatum ptrD
			   	(cptrP,sizeP) <- peekDatum ptrP
			   	(cptrQ,sizeQ) <- peekDatum ptrQ
			   	(cptrU,sizeU) <- peekDatum ptrU
				fpM <- newForeignPtr c_free_finalizer (castPtr cptrM)
				fpE <- newForeignPtr c_free_finalizer (castPtr cptrE)
				fpD <- newForeignPtr c_free_finalizer (castPtr cptrD)
				fpP <- newForeignPtr c_free_finalizer (castPtr cptrP)
				fpQ <- newForeignPtr c_free_finalizer (castPtr cptrQ)
				fpU <- newForeignPtr c_free_finalizer (castPtr cptrU)
				return $ Right
					(fromForeignPtr fpM sizeM
					,fromForeignPtr fpE sizeE
					,fromForeignPtr fpD sizeD
					,fromForeignPtr fpP sizeP
					,fromForeignPtr fpQ sizeQ
					,fromForeignPtr fpU sizeU)

getPeersX509DN :: Certificate -> IO String
getPeersX509DN cert = do
  withCertificate cert $ \cp ->
      alloca $ \lenp -> do
        res <- {#call unsafe gnutls_x509_crt_get_dn #} cp nullPtr lenp
        len <- peek lenp
        if (len == 0)
           then return []
           else allocaBytes (fromIntegral len) $ \charp -> do
                  res2 <- {#call unsafe gnutls_x509_crt_get_dn #} cp charp lenp
                  throwGnutlsIfNeg res2
                  len2 <- peek lenp
                  peekCStringLen (charp, fromIntegral len2)


-- | Check whether the certicate hostname matches the given name.
{#fun pure unsafe gnutls_x509_crt_check_hostname as checkHostname
 {withCertificate* `Certificate', withCString* `String'} -> `GnuTLSMonad Bool' gnuTLSCheckBool #}

-- | Check if the second certificate issued the first one.
{#fun pure unsafe gnutls_x509_crt_check_issuer as checkIssuer
 {withCertificate* `Certificate',withCertificate* `Certificate'} -> `GnuTLSMonad Bool' gnuTLSCheckBool#}

activationTime :: ReadWriteAttr Certificate ClockTime ClockTime
activationTime = newAttr g s
    where g c = withCertificate c {#call unsafe gnutls_x509_crt_get_activation_time#} >>= return . integralToClockTime
          s c (TOD s _) = do withCertificate c $ \cp -> do
                             {#call gnutls_x509_crt_set_activation_time #} cp (fromIntegral s) >>= throwGnutlsIf

expirationTime :: ReadWriteAttr Certificate ClockTime ClockTime
expirationTime = newAttr g s
    where g c = withCertificate c {#call unsafe gnutls_x509_crt_get_expiration_time#} >>= return . integralToClockTime
          s c (TOD s _) = do withCertificate c $ \cp -> do
                             {#call unsafe gnutls_x509_crt_set_expiration_time #} cp (fromIntegral s) >>= throwGnutlsIf

getIssuerDnByOid :: Certificate -> OID -> Int -> GnuTLSMonad (Maybe ByteString)
getIssuerDnByOid cert str ind = unsafePerformIO (getDnByOid' {#call unsafe gnutls_x509_crt_get_issuer_dn_by_oid#} cert str ind)

-- | Retrieves a field by it\'s Object Identifier.
getDnByOid :: Certificate	-- ^ Certificate to retrieve the field-data from 
	   -> OID		-- ^ The field name, specified by an Object Indentifier
	   -> Int		-- ^ If there\'s more than one entry, this will be used to describe which one to use(0 gives the first entry)
	   -> GnuTLSMonad (Maybe ByteString)
getDnByOid cert str ind = unsafePerformIO (getDnByOid' {#call unsafe gnutls_x509_crt_get_dn_by_oid#} cert str ind)

getExtensionByOid :: Certificate
		  -> OID
		  -> Int
		  -> GnuTLSMonad (Maybe (ByteString,Bool))
getExtensionByOid cert str ind
	= unsafePerformIO $ alloca $ \crit ->
	  withCString str $ \cstr -> 
	  withCertificate cert $ \certptr ->
	  with (1024::Int) $ \szptr ->
	  createAndTrim' 1024 (\ptr -> do
	  	res <- {#call unsafe gnutls_x509_crt_get_extension_by_oid#} certptr cstr (fromIntegral ind) (castPtr ptr) (castPtr szptr) crit
		if res /= 0
			then return (0,0,Left $ GnuTLSError res)
			else (if ptr == nullPtr
				then return $ (0,0,Right False)
				else (do
					size <- peek szptr
					return (0,size,Right True)
					))) >>= \(bs,res) -> case res of
						Left err -> return $ Left err
						Right True -> do
							iscrit <- peek crit
							return $ Right $ Just (bs,iscrit < 0)
						Right False -> return $ Right Nothing

getDnByOid' f cert str ind
	= withCString str $ \cstr -> 
	  withCertificate cert $ \certptr ->
	  with (1024::Int) $ \szptr ->
	  createAndTrim' 1024 (\ptr -> do
	  	res <- f certptr cstr (fromIntegral ind) 0 (castPtr ptr) (castPtr szptr)
		if res /= 0
			then return (0,0,Left $ GnuTLSError res)
			else (if ptr == nullPtr
				then return $ (0,0,Right False)
				else (do
					size <- peek szptr
					return (0,size,Right True)
					))) >>= \(bs,res) -> return $ case res of
						Left err -> Left err
						Right True -> Right $ Just bs
						Right False -> Right Nothing

privateKeyGetKeyId :: PrivateKey -> GnuTLSMonad ByteString
privateKeyGetKeyId key = unsafePerformIO $
	withPrivateKey key $ \rkey ->
	with (20::Int) $ \szptr ->  -- KeyID is a SHA1 hash, ergo always 20 bytes
	createAndTrim' 20 (\ptr -> do
		res <- {#call unsafe gnutls_x509_privkey_get_key_id#} rkey 0 (castPtr ptr) (castPtr szptr)
		if res /= 0
			then return (0,0,Just $ GnuTLSError res)
			else (do
				size <- peek szptr
				return (0,size,Nothing)
				)) >>= \(bs,res) -> case res of
					Nothing -> return (Right bs)
					Just err -> return (Left err)

certificateGetKeyId :: Certificate -> GnuTLSMonad ByteString
certificateGetKeyId cert = unsafePerformIO $
	withCertificate cert $ \rcert ->
	with (20::Int) $ \szptr ->
	createAndTrim' 20 (\ptr -> do
		res <- {#call unsafe gnutls_x509_crt_get_key_id#} rcert 0 (castPtr ptr) (castPtr szptr)
		if res/=0
			then return (0,0,Just $ GnuTLSError res)
			else (do
				size <- peek szptr
				return (0,size,Nothing)
				)) >>= \(bs,res) -> case res of
					Nothing -> return (Right bs)
					Just err -> return (Left err)
