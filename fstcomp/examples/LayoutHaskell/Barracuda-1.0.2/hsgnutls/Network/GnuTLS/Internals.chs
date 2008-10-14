
module Network.GnuTLS.Internals where

#include <gnutls/gnutls.h>

{#context prefix = "gnutls" #}

import Data.IORef
import Foreign
import Foreign.C
import System.Time(ClockTime(TOD))
import Network.GnuTLS.RefCount
import Data.ByteString.Base

data Session a = Session (ForeignPtr ())! RefCount

--------------------------------------------------------------------------------
------------------------------   Enums   ---------------------------------------
--------------------------------------------------------------------------------

{#enum gnutls_cipher_algorithm_t as CipherAlgorithm {underscoreToCase} #}
{#enum gnutls_kx_algorithm_t as KxAlgorithm {underscoreToCase} deriving(Eq) #}
{#enum gnutls_params_type_t as ParamsType {underscoreToCase} #}
{#enum gnutls_credentials_type_t as CredentialsType {underscoreToCase} deriving(Eq) #}
{#enum gnutls_mac_algorithm_t as MacAlgorithm {underscoreToCase} #}
{#enum gnutls_digest_algorithm_t as DigestAlgorithm {underscoreToCase} deriving(Show) #}
{#enum gnutls_compression_method_t as CompressionMethod {underscoreToCase} #}
{#enum gnutls_connection_end_t as ConnectionEnd {underscoreToCase} #}
{#enum gnutls_alert_level_t as AlertLevel {underscoreToCase} #}
{#enum gnutls_alert_description_t as AlertDescription {underscoreToCase} #}
{#enum gnutls_handshake_description_t as HandshakeDescription {underscoreToCase} #}
{#enum gnutls_certificate_status_t as CertificateStatus {underscoreToCase} deriving(Show,Eq) #}
{#enum gnutls_certificate_request_t as CertificateRequest {underscoreToCase} #}
{#enum gnutls_openpgp_key_status_t as KeyStatus {underscoreToCase} #}
{#enum gnutls_close_request_t as CloseRequest {underscoreToCase} #}
{#enum gnutls_protocol_t as Protocol {underscoreToCase} #}
{#enum gnutls_certificate_type_t as CertificateType {underscoreToCase} #}
{#enum gnutls_x509_crt_fmt_t as X509CertificateFormat {underscoreToCase} #}
{#enum gnutls_pk_algorithm_t as PkAlgorithm {underscoreToCase} #}
{#enum gnutls_sign_algorithm_t as SignAlgorithm {underscoreToCase} #}

#c
#ifdef COMPAT_GNUTLS_1_0
typedef gnutls_cipher_algorithm gnutls_cipher_algorithm_t;
typedef gnutls_kx_algorithm gnutls_kx_algorithm_t;
typedef gnutls_mac_algorithm gnutls_mac_algorithm_t;
typedef gnutls_digest_algorithm gnutls_digest_algorithm_t;
typedef gnutls_datum gnutls_datum_t;
typedef gnutls_params_type gnutls_params_type_t;
typedef gnutls_credentials_type gnutls_credentials_type_t;
typedef gnutls_compression_method gnutls_compression_method_t;
typedef gnutls_connection_end gnutls_connection_end_t;
typedef gnutls_alert_level gnutls_alert_level_t;
typedef gnutls_alert_description gnutls_alert_description_t;
typedef gnutls_handshake_description gnutls_handshake_description_t;
typedef gnutls_certificate_status gnutls_certificate_status_t;
typedef gnutls_certificate_request gnutls_certificate_request_t;
typedef gnutls_openpgp_key_status gnutls_openpgp_key_status_t;
typedef gnutls_close_request gnutls_close_request_t;
typedef gnutls_protocol_version gnutls_protocol_t;
typedef gnutls_certificate_type gnutls_certificate_type_t;
typedef gnutls_x509_crt_fmt gnutls_x509_crt_fmt_t;
typedef gnutls_pk_algorithm gnutls_pk_algorithm_t;
typedef gnutls_sign_algorithm gnutls_sign_algorithm_t;
#endif
#endc


--------------------------------------------------------------------------------
----------------------------   Time Management   -------------------------------
--------------------------------------------------------------------------------

integralToClockTime :: Integral n => n -> ClockTime
integralToClockTime ct = TOD (fromIntegral ct) 0

--------------------------------------------------------------------------------
-----------------------------   Helpers   --------------------------------------
--------------------------------------------------------------------------------

enumCInt :: Enum e => e -> CInt
enumCInt x = fromIntegral $ fromEnum x
cintEnum :: Enum e => CInt -> e
cintEnum x = toEnum $ fromIntegral x

safePeekCString :: CString -> IO String
safePeekCString ptr = if ptr == nullPtr then return "" else peekCString ptr

withEnumList0 :: Enum e => [e] -> (Ptr CInt -> IO ()) -> IO ()
withEnumList0 es f = withArray0 0 is f
    where is = map enumCInt es

withSession :: Session t -> (Ptr () -> IO a) -> IO a
withSession (Session s _) = withForeignPtr s

{-# SPECIALIZE throwGnutlsIf :: CInt  -> IO () #-}
{-# SPECIALIZE throwGnutlsIf :: CLong -> IO () #-}

throwGnutlsIf :: Integral n => n -> IO ()
throwGnutlsIf 0     = return ()
throwGnutlsIf v     = {#call gnutls_strerror #} (fromIntegral v) >>= safePeekCString >>= (\str -> fail (str++" ("++show v++")"))

{-# SPECIALIZE throwGnutlsIfNeg :: CInt  -> IO Int #-}
{-# SPECIALIZE throwGnutlsIfNeg :: CLong -> IO Int #-}
throwGnutlsIfNeg v = if v < 0 then throwGnutlsIf v >> return 0 else return (fromIntegral v)

{-# INLINE peekEnum #-}
peekEnum :: (Storable s, Integral s, Num e, Enum e) => Ptr s -> IO e
peekEnum ptr = peek ptr >>= return . fromIntegral

isZero x = x == 0

isNonZero x = x /= 0

ptrDeS :: Ptr (Ptr ()) -> IO a
ptrDeS p = deRefStablePtr $ castPtrToStablePtr $ castPtr p

--------------------------------------------------------------------------------
------------------------------   Datums   --------------------------------------
--------------------------------------------------------------------------------

peekDatum :: Ptr a -> IO (Ptr CChar,Int)
peekDatum ptr = do pv <- {#get gnutls_datum_t->data #} (castPtr ptr)
                   iv <- {#get gnutls_datum_t->size #} (castPtr ptr)
                   return (castPtr pv,fromIntegral iv)

peekDatumArray :: Int -> Ptr a -> IO [(Ptr CChar,Int)]
peekDatumArray i ptr = loop i (plusPtr ptr (i*{#sizeof gnutls_datum_t #})) []
    where loop 0 ptr acc = do d <- peekDatum ptr
                              return (d:acc)
          loop k ptr acc = do d <- peekDatum ptr
                              loop (k-1) (plusPtr ptr (0-{#sizeof gnutls_datum_t #})) (d:acc)

class Datum a where 
  withDatum :: a -> (Ptr () -> IO b) -> IO b

instance Datum String where
  withDatum s p = withCStringLen s (\v -> withDatum v p)

instance Datum (Ptr CChar,Int) where 
  withDatum (p,l) c = allocaBytes {#sizeof gnutls_datum_t #} $ \dptr -> do
                      {#set gnutls_datum_t->data #} dptr (castPtr p)
                      {#set gnutls_datum_t->size #} dptr (fromIntegral l)
                      c dptr

instance Datum ByteString where
  withDatum bs f = unsafeUseAsCStringLen bs (\v -> withDatum v f)

--{#fun gnutls_pem_base64_decode_alloc as datumBase64Decode
--  {with
datumBase64Decode :: (Datum d) => Int -> d -> IO ByteString
datumBase64Decode sz dat = withDatum dat $
	\p1 -> createAndTrim sz $
	\ptr -> with (fromIntegral sz::CSize) $
	\sptr -> do
		{#call gnutls_pem_base64_decode#} nullPtr p1 (castPtr ptr) (castPtr sptr) >>= throwGnutlsIf
		peek sptr >>= return.fromIntegral

