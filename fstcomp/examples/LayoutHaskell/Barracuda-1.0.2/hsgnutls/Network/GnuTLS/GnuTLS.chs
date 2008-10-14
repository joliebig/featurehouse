
module Network.GnuTLS.GnuTLS 
    (-- * Enumerations
     CipherAlgorithm(..), KxAlgorithm(..), ParamsType(..), CredentialsType(..),
     MacAlgorithm(..), DigestAlgorithm(..), CompressionMethod(..), ConnectionEnd(..),
     AlertLevel(..), AlertDescription(..), HandshakeDescription(..), CertificateStatus(..), 
     CertificateRequest(..), KeyStatus(..), CloseRequest(..), Protocol(..), 
     CertificateType(..), X509CertificateFormat(..), PkAlgorithm(..), SignAlgorithm(..), 
     -- * Types
     Session, DH, RSA, AnonymousServerCredentials, AnonymousClientCredentials,
     Server, Client, Transport, CertificateCredentials,Datum(..),
     -- * Priority
     SetPriority(..), setDefaultPriority, setDefaultExportPriority,
     -- * Credentials
     SetCredentials(..), Clear(..),
     -- * Creating connections
     tlsClient, tlsServer,
     -- * Session Accessors
     priorities, credentials, dhPrimeBits, transport, handle, clientCert,
#ifdef NETWORK_ALT
     sock,
#endif
    -- * Managing connection
    handshake, rehandshake, bye, 
    setMaxHandshakePacketLength,
    -- * Querying connection attributes
    serverWantedAuth, verifyPeer, isResumed, 
    getAlert, getCipher, getKx, getMac, getCompression, getCertificateType, getProtocol, 
    getCredentialsType,
#ifndef COMPAT_GNUTLS_1_0
    getServerCredentialsType, getClientCredentialsType,
#endif
    getPeersCertificatesRaw,
    -- * Sending and receiving data
    tlsSend, tlsSendString, tlsRecv, tlsRecvString, tlsCheckPending,
    -- * Certificate functions
    certificateCredentials, freeKeys, freeCas, freeCaNames, freeCrls,
    certificateTrustFile, certificateCrlFile, certificateKeyFile,
    -- * Miscellaneous
     anonymousServerCredentials, anonymousClientCredentials,
     SetDHParams(..), CredParameter(..),
     newDH, newRSA,cipherKeySize,cipherSuiteName,
     version, gnutlsGlobalInit 
    ) where

import Control.Concurrent
import Data.Bits((.&.), (.|.))
import Data.IORef(newIORef, readIORef)
import Foreign
import Foreign.C
import qualified Foreign.Concurrent as FC
import Foreign.Ptr
import System.IO
import System.IO.Unsafe
import Network.GnuTLS.Attributes
import Network.GnuTLS.Internals
import Network.GnuTLS.RefCount
#ifdef NETWORK_ALT
import Network.Alt
#endif

#include <gnutls/gnutls.h>

{#context prefix = "gnutls" #}


--------------------------------------------------------------------------------
------------------------------   Types   ---------------------------------------
--------------------------------------------------------------------------------

data DH  = DH  (ForeignPtr ())! RefCount
data RSA = RSA (ForeignPtr ())! RefCount
data AnonymousServerCredentials = ASC (ForeignPtr ())! RefCount
data AnonymousClientCredentials = ACC (ForeignPtr ())! RefCount
data CertificateCredentials     = CC  (ForeignPtr ())! RefCount

data Server
data Client
type Transport = Ptr () -> Int -> Ptr CInt -> IO Int

withDH  (DH fp _)  = withForeignPtr fp
withRSA (RSA fp _) = withForeignPtr fp
withAnonymousServerCredentials (ASC fp _) = withForeignPtr fp
withAnonymousClientCredentials (ACC fp _) = withForeignPtr fp
withCertificateCredentials     (CC fp _)  = withForeignPtr fp


--------------------------------------------------------------------------------
-------------------------   Show Functions   -----------------------------------
--------------------------------------------------------------------------------

instance Show AlertDescription where
    show a = unsafePerformIO $ gnutlsAlertGetName a
{#fun unsafe gnutls_alert_get_name as ^ {enumCInt `AlertDescription'} -> `String' safePeekCString* #}

instance Show CipherAlgorithm where
    show a = unsafePerformIO $ gnutlsCipherGetName a
{#fun unsafe gnutls_cipher_get_name as ^ {enumCInt `CipherAlgorithm'} -> `String' safePeekCString* #}

instance Show MacAlgorithm where
    show a = unsafePerformIO $ gnutlsMacGetName a
{#fun unsafe gnutls_mac_get_name as ^ {enumCInt `MacAlgorithm'} -> `String' safePeekCString* #}

instance Show CompressionMethod where
    show a = unsafePerformIO $ gnutlsCompressionGetName a
{#fun unsafe gnutls_compression_get_name as ^ {enumCInt `CompressionMethod'} -> `String' safePeekCString* #}

instance Show KxAlgorithm where
    show a = unsafePerformIO $ gnutlsKxGetName a
{#fun unsafe gnutls_kx_get_name as ^ {enumCInt `KxAlgorithm'} -> `String' safePeekCString* #}

instance Show CertificateType where
    show a = unsafePerformIO $ gnutlsCertificateTypeGetName a
{#fun unsafe gnutls_certificate_type_get_name as ^ {enumCInt `CertificateType'} -> `String' safePeekCString* #}

instance Show Protocol where
    show a = unsafePerformIO $ gnutlsProtocolGetName a
{#fun unsafe gnutls_protocol_get_name as ^ {enumCInt `Protocol'} -> `String' safePeekCString* #}

instance Show PkAlgorithm where
    show a = unsafePerformIO $ gnutlsPkAlgorithmGetName a
{#fun unsafe gnutls_pk_algorithm_get_name as ^ {enumCInt `PkAlgorithm'} -> `String' safePeekCString* #}

instance Show SignAlgorithm where
    show a = unsafePerformIO $ gnutlsSignAlgorithmGetName a
{#fun unsafe gnutls_sign_algorithm_get_name as ^ {enumCInt `SignAlgorithm'} -> `String' safePeekCString* #}

--------------------------------------------------------------------------------
--------------------------   Set Priority   ------------------------------------
--------------------------------------------------------------------------------

setpwrap :: Enum e => (Ptr () -> Ptr CInt -> IO CInt) -> Session t -> [e] -> IO ()
setpwrap comp s x = do
  withEnumList0 x $ \e -> do
  withSession s   $ \p -> do
  comp p e >>= throwGnutlsIf

-- | Set the priority of the specified category.
-- On servers this means the set of acceptable values,
-- on clients it sets the priorities.
class SetPriority a where setPriority :: Session t -> [a] -> IO ()

instance SetPriority CipherAlgorithm where 
  setPriority = setpwrap {#call unsafe gnutls_cipher_set_priority #}

instance SetPriority MacAlgorithm where 
  setPriority = setpwrap {#call unsafe gnutls_mac_set_priority #}

instance SetPriority CompressionMethod where 
  setPriority = setpwrap {#call unsafe gnutls_compression_set_priority #}

instance SetPriority KxAlgorithm where 
  setPriority = setpwrap {#call unsafe gnutls_kx_set_priority #}

instance SetPriority Protocol where 
  setPriority = setpwrap {#call unsafe gnutls_protocol_set_priority #}

instance SetPriority CertificateType where 
  setPriority = setpwrap {#call unsafe gnutls_certificate_type_set_priority #}

-- | Set default priorities. This is called whenever a new 'Session' is created to 
-- ensure sensible defaults. 
{#fun gnutls_set_default_priority as setDefaultPriority {withSession* `Session t'} -> `()' #}
-- | Set default priorities conforming with various export regulations.
-- /Includes weak algorithms./
{#fun gnutls_set_default_export_priority as setDefaultExportPriority {withSession* `Session t'} -> `()' #}


--------------------------------------------------------------------------------
------------------------   Setting Credentials   -------------------------------
--------------------------------------------------------------------------------

-- | Set the credentials associated with a session.
class SetCredentials st a where setCredentials :: Session st -> a -> IO ()
instance SetCredentials st a => SetCredentials st (IO a) where 
  setCredentials s c = setCredentials s =<< c
instance SetCredentials Server AnonymousServerCredentials where 
  setCredentials s (ASC a rc) = setcred_ s CrdAnon (castForeignPtr a)
instance SetCredentials Client AnonymousClientCredentials where 
  setCredentials s (ACC a rc) = setcred_ s CrdAnon (castForeignPtr a)
instance SetCredentials t CertificateCredentials where 
  setCredentials s (CC a rc) = setcred_ s CrdCertificate (castForeignPtr a)

setcred_ :: Session t -> CredentialsType -> ForeignPtr a -> IO ()
setcred_ (Session sfp refc) ct fp = do
  withForeignPtr sfp $ \sptr ->     do
  withForeignPtr fp  $ \dptr ->     do
  {#call gnutls_credentials_set #} sptr (enumCInt ct) (castPtr dptr) >>= throwGnutlsIf
  addRefFinalizer refc (touchForeignPtr fp)

-- | Used to clear all credentials associated with a session.
data Clear = Clear
instance SetCredentials t Clear where
  setCredentials s _ = withSession s (\s -> {#call gnutls_credentials_clear #} s >> return ())

--------------------------------------------------------------------------------
---------------------------   Session Creation   -------------------------------
--------------------------------------------------------------------------------

type Prop a = AttrOp IO a

tlsClient :: [Prop (Session Client)] -> IO (Session Client)
tlsClient p = do s <- newSession Client
                 set s p
                 return s

tlsServer :: [Prop (Session Server)] -> IO (Session Server)
tlsServer p = do s <- newSession Server
                 set s p
                 return s


newSession :: ConnectionEnd -> IO (Session t)
newSession ce = gnutlsGlobalInit >> alloca (\err -> do
  let efun b l e = putStrLn "TLS pull/push functions not set" >> return 0
  ef1 <- newStablePtr efun
  ef2 <- newStablePtr efun
  sp <- init_session_wrap (enumCInt ce) ef1 ef2 err
  peek err >>= throwGnutlsIf
  rc <- newRefCount (replace_transport_stable_ptrs sp stableNull stableNull >> gnutls_deinit sp)
  ps <- FC.newForeignPtr sp (freeRef rc)
  return $ Session ps rc)

stableNull = castPtrToStablePtr nullPtr

foreign import ccall unsafe "TLS.h gnutls_init" gnutls_init :: Ptr (Ptr ()) -> CInt -> IO CInt
foreign import ccall unsafe "TLS.h gnutls_deinit" gnutls_deinit :: Ptr () -> IO ()

foreign import ccall unsafe "TLS.h init_session_wrap"
  init_session_wrap :: CInt -> StablePtr Transport -> StablePtr Transport -> Ptr CInt -> IO (Ptr ())
#c
#ifdef COMPAT_GNUTLS_1_0
typedef gnutls_transport_ptr gnutls_transport_ptr_t;
typedef gnutls_connection_end gnutls_connection_end_t;
typedef gnutls_session gnutls_session_t;
typedef gnutls_dh_params gnutls_dh_params_t;
typedef gnutls_rsa_params gnutls_rsa_params_t;
typedef gnutls_anon_server_credentials gnutls_anon_server_credentials_t;
typedef gnutls_anon_client_credentials gnutls_anon_client_credentials_t;
typedef gnutls_certificate_credentials gnutls_certificate_credentials_t;
#endif
gnutls_session_t init_session_wrap(gnutls_connection_end_t con_end, void *sptr1, void *sptr2, int *err);
#endc

--------------------------------------------------------------------------------
-------------------------   Session Accessors   --------------------------------
--------------------------------------------------------------------------------

priorities :: SetPriority a => WriteAttr (Session t) [a]
priorities = writeAttr setPriority

credentials :: SetCredentials t a => WriteAttr (Session t) a
credentials = writeAttr setCredentials

dhPrimeBits :: ReadWriteAttr (Session t) Int Int
dhPrimeBits = newAttr get set
    where get s   = withSession s {#call gnutls_dh_get_prime_bits #}  >>= return . fromIntegral
          set s v = withSession s (\sp -> {#call gnutls_dh_set_prime_bits #} sp (fromIntegral v))


transport :: ReadWriteAttr (Session t) (Transport,Transport) (Transport,Transport)
transport = newAttr gnutls_transport_get_ptr2 set
    where set s (a,b) =  do a' <- newStablePtr a
                            b' <- newStablePtr b
                            withSession s $ \sp -> replace_transport_stable_ptrs sp a' b'

{#fun unsafe gnutls_transport_get_ptr2 
  {withSession* `Session t',
   alloca- `Transport' ptrDeS*,
   alloca- `Transport' ptrDeS*}
  -> `()' #}

foreign import ccall "replace_transport_stable_ptrs"
  replace_transport_stable_ptrs :: Ptr () -> StablePtr Transport -> StablePtr Transport -> IO ()

#c
void replace_transport_stable_ptrs(gnutls_session_t session, void *p1, void *p2);
#endc

#ifdef NETWORK_ALT

sock :: WriteAttr (Session t) Socket
sock = writeAttr ss
    where rf sfd buf len err       = recv sfd buf len `catch` ef err
          sf sfd buf len err       = send sfd buf len `catch` ef err
          ef err _                 = do (Errno iv) <- getErrno; poke err iv; return (-1)
          ss (Session sfp sr) sfd  = do rf' <- newStablePtr $ rf sfd
                                        sf' <- newStablePtr $ sf sfd
                                        withForeignPtr sfp $ \p -> replace_transport_stable_ptrs p rf' sf'
                                        addRefFinalizer sr (close sfd)
#endif /* NETWORK_ALT */

handle :: WriteAttr (Session t) Handle
handle = writeAttr ss
    where rf hdl buf len err       = (hGetBuf hdl buf (fromIntegral len)) `catch` ef err
          sf hdl buf len err       = (hPutBuf hdl buf (fromIntegral len) >> return len) `catch` ef err
          ef err _                 = do (Errno iv) <- getErrno; poke err iv; return (-1)
          ss (Session sfp sr) hdl  = do rf' <- newStablePtr $ rf hdl
                                        sf' <- newStablePtr $ sf hdl
                                        hSetBuffering hdl NoBuffering
                                        withForeignPtr sfp $ \p -> replace_transport_stable_ptrs p rf' sf'
                                        addRefFinalizer sr (hClose hdl)


--------------------------------------------------------------------------------
-----------------------------  Session Actions   -------------------------------
--------------------------------------------------------------------------------

-- | Terminates the current TLS connection, which has been succesfully established
-- with 'handshake'. Notifies the peer with an alert that the connection is closing.
{#fun gnutls_bye as bye {withSession* `Session t',enumCInt `CloseRequest'} -> `()' throwGnutlsIf* #}
-- | Perform a handshake with the peer and initialize a TLS\/SSL connection.
-- Note that after the handshake completes applications /must/ check 
-- whether a high enough level of confidentiality was established.
{#fun gnutls_handshake as handshake {withSession* `Session t'} -> `' throwGnutlsIf* #}
-- | Tells the client that we want to renogotiate the handshake. If the function
-- succeeds then 'handshake' can be called again on the connection.
{#fun gnutls_rehandshake as rehandshake {withSession* `Session Server'} -> `' throwGnutlsIf* #}

-- | Set the maximum size of a handshake request. Larger requests are ignored.
-- Defaults to 16kb which should be large enough.
{#fun gnutls_handshake_set_max_packet_length as setMaxHandshakePacketLength 
  {withSession* `Session t', fromIntegral `Int'} -> `()' #}

-- | Set whether we want to do client authentication.
clientCert :: WriteAttr (Session Server) CertificateRequest
clientCert = writeAttr p
    where p s v = withSession s $ \sp -> {#call unsafe gnutls_certificate_server_set_request #} sp (enumCInt v)

--------------------------------------------------------------------------------
----------------------------  Session Getters   --------------------------------
--------------------------------------------------------------------------------
 
serverWantedAuth :: Session Client -> IO Bool
serverWantedAuth s = withSession s $ \sp -> do
  iv <- {#call gnutls_certificate_client_get_request_status #} sp
  throwGnutlsIfNeg iv
  return (iv /= 0)

verifyPeer :: Session t -> IO [CertificateStatus]
verifyPeer ses = do 
  withSession ses $ \sp -> do
  alloca $ \stat -> do
  {#call gnutls_certificate_verify_peers2 #} sp stat >>= throwGnutlsIf
  ci <- peekEnum stat
  return $ filter (\e -> fromEnum e .&. ci /= 0) [CertInvalid, CertRevoked, CertSignerNotFound, CertSignerNotCa]

-- | Test whether this session is a resumed one.
{#fun gnutls_session_is_resumed as isResumed {withSession* `Session t'} -> `Bool' isNotZero #}

isNotZero :: CInt -> Bool
isNotZero 0 = False
isNotZero _ = True

-- | Return the value of the last alert received - undefined if no alert has been received.
{#fun unsafe gnutls_alert_get            as getAlert        {withSession* `Session t'} -> `AlertDescription' cintEnum #}
-- | Return the currently used cipher.
{#fun unsafe gnutls_cipher_get           as getCipher       {withSession* `Session t'} -> `CipherAlgorithm' cintEnum #}
-- | Return the key exchange algorithm used in the last handshake.
{#fun unsafe gnutls_kx_get               as getKx           {withSession* `Session t'} -> `KxAlgorithm' cintEnum #}
-- | Return the mac algorithm currently used.
{#fun unsafe gnutls_mac_get              as getMac          {withSession* `Session t'} -> `MacAlgorithm' cintEnum #}
-- | Return the compression method currently used.
{#fun unsafe gnutls_compression_get      as getCompression  {withSession* `Session t'} -> `CompressionMethod' cintEnum #}
-- | Return the currently used certificate type.
{#fun unsafe gnutls_certificate_type_get as getCertificateType  {withSession* `Session t'} -> `CertificateType' cintEnum #}
-- | Return the currently used protocol version.
{#fun unsafe gnutls_protocol_get_version as getProtocol     {withSession* `Session t'} -> `Protocol' cintEnum #}

-- | Return type of credentials for the current authentication schema.
{#fun gnutls_auth_get_type as getCredentialsType {withSession* `Session t'} -> `CredentialsType' cintEnum #}

#ifndef COMPAT_GNUTLS_1_0
-- | Return the type of credentials used for authenticating the server. Available with GnuTLS 1.2.
{#fun gnutls_auth_server_get_type as getServerCredentialsType {withSession* `Session t'} -> `CredentialsType' cintEnum #}
-- | Return the type of credentials used for authenticating the client. Available with GnuTLS 1.2.
{#fun gnutls_auth_client_get_type as getClientCredentialsType {withSession* `Session t'} -> `CredentialsType' cintEnum #}
#endif

-- | Get the certificate chain of the peer. 
-- In the case of X509 will return DER encoded certificate list
-- beginning with the peers key and continuing in the issuer chain.
-- With OpenPGP a single key will be returned in the raw format.
getPeersCertificatesRaw :: Session t -> IO [(Ptr CChar,Int)]
getPeersCertificatesRaw ses = do
  withSession ses $ \sp ->
    alloca $ \lenp -> do
      res <- {#call gnutls_certificate_get_peers #} sp lenp
      if res == nullPtr 
         then return []
         else do len <- peek lenp
                 peekDatumArray (fromIntegral len - 1) res

--------------------------------------------------------------------------------
----------------------------  Read/Write Data   --------------------------------
--------------------------------------------------------------------------------

{#fun gnutls_record_send as tlsSend 
  {withSession* `Session t', castPtr `Ptr a', fromIntegral `Int' } -> `Int' throwGnutlsIfNeg* #}

tlsSendString :: Session t -> String -> IO ()
tlsSendString ses str = withCStringLen str $ \(ptr,len) -> loop ptr len
    where loop ptr len = do r <- tlsSend ses ptr len
                            case len-r of
                              x | x > 0 -> loop (plusPtr ptr r) x
                                | True  -> return ()
  
{#fun gnutls_record_check_pending as tlsCheckPending
  {withSession* `Session t'} -> `Int' fromIntegral #}

{#fun gnutls_record_recv as tlsRecv
  {withSession* `Session t', castPtr `Ptr a', fromIntegral `Int'} -> `Int' throwGnutlsIfNeg* #}

tlsRecvString :: Session t -> IO String
tlsRecvString ses = allocaBytes 1024 $ \ptr -> do
  r <- tlsRecv ses ptr 1024
  peekCAStringLen (ptr,r)

--------------------------------------------------------------------------------
---------------------   Anonymous Server Credentials   -------------------------
--------------------------------------------------------------------------------

anonymousServerCredentials :: IO AnonymousServerCredentials
anonymousServerCredentials = alloca $ \ptr -> do
  gnutls_anon_allocate_server_credentials ptr >>= throwGnutlsIf
  raw<- peek ptr
  rc <- newRefCount $ gnutls_anon_free_server_credentials raw
  fp <- FC.newForeignPtr raw $ freeRef rc
  return $ ASC fp rc

foreign import ccall safe "TLS.h gnutls_anon_free_server_credentials"
  gnutls_anon_free_server_credentials :: Ptr () -> IO ()

foreign import ccall safe "TLS.h gnutls_anon_allocate_server_credentials" 
  gnutls_anon_allocate_server_credentials :: Ptr (Ptr ()) -> IO CInt

class SetDHParams a where setDHParams :: a -> DH -> IO ()
instance SetDHParams AnonymousServerCredentials where 
  setDHParams asc@(ASC _ rc) dh@(DH _ refc) =
      do gnutls_anon_set_server_dh_params asc dh
         allocRef refc
         addRefFinalizer rc $ freeRef refc

{#fun gnutls_anon_set_server_dh_params 
  {withAnonymousServerCredentials* `AnonymousServerCredentials', withDH* `DH'} -> `()' #}

--------------------------------------------------------------------------------
---------------------   Anonymous Client Credentials   -------------------------
--------------------------------------------------------------------------------

anonymousClientCredentials :: IO AnonymousClientCredentials
anonymousClientCredentials = alloca $ \ptr -> do
  gnutls_anon_allocate_client_credentials ptr >>= throwGnutlsIf
  raw<- peek ptr
  rc <- newRefCount $ gnutls_anon_free_client_credentials raw
  fp <- FC.newForeignPtr raw $ freeRef rc
  return $ ACC fp rc

foreign import ccall safe "TLS.h gnutls_anon_free_client_credentials"
  gnutls_anon_free_client_credentials :: Ptr () -> IO ()

foreign import ccall safe "TLS.h gnutls_anon_allocate_client_credentials" 
  gnutls_anon_allocate_client_credentials :: Ptr (Ptr ()) -> IO CInt

--------------------------------------------------------------------------------
-----------------------   Certificate Credentials   ----------------------------
--------------------------------------------------------------------------------

certificateCredentials :: IO CertificateCredentials
certificateCredentials = alloca $ \ptr -> do
  gnutls_certificate_allocate_credentials ptr >>= throwGnutlsIf
  raw<- peek ptr
  rc <- newRefCount $ gnutls_certificate_free_credentials raw
  fp <- FC.newForeignPtr raw $ freeRef rc
  return $ CC fp rc

foreign import ccall safe "TLS.h gnutls_certificate_free_credentials" 
  gnutls_certificate_free_credentials :: Ptr () -> IO ()

foreign import ccall safe "TLS.h gnutls_certificate_allocate_credentials" 
  gnutls_certificate_allocate_credentials :: Ptr (Ptr ()) -> IO CInt

{#fun gnutls_certificate_free_keys as freeKeys {withCertificateCredentials* `CertificateCredentials'} -> `()' #}
{#fun gnutls_certificate_free_cas as freeCas {withCertificateCredentials* `CertificateCredentials'} -> `()' #}
{#fun gnutls_certificate_free_ca_names as freeCaNames {withCertificateCredentials* `CertificateCredentials'} -> `()' #}
{#fun gnutls_certificate_free_crls as freeCrls {withCertificateCredentials* `CertificateCredentials'} -> `()' #}

instance SetDHParams CertificateCredentials where 
  setDHParams cc@(CC _ rc) dh@(DH _ refc) =
      do gnutls_certificate_set_dh_params cc dh
         allocRef refc
         addRefFinalizer rc $ freeRef refc

{#fun gnutls_certificate_set_dh_params 
  {withCertificateCredentials* `CertificateCredentials', withDH* `DH'} -> `()' #}

{#fun gnutls_certificate_set_x509_trust_file as certificateTrustFile
  {withCertificateCredentials* `CertificateCredentials', 
   withCAString* `FilePath',
   enumCInt `X509CertificateFormat'}
  -> `Int' throwGnutlsIfNeg* #}

{#fun gnutls_certificate_set_x509_crl_file as certificateCrlFile
  {withCertificateCredentials* `CertificateCredentials', 
   withCAString* `FilePath',
   enumCInt `X509CertificateFormat'}
  -> `Int' throwGnutlsIfNeg* #}

{#fun gnutls_certificate_set_x509_key_file as certificateKeyFile
  {withCertificateCredentials* `CertificateCredentials', 
   withCAString* `FilePath',
   withCAString* `FilePath',
   enumCInt `X509CertificateFormat'}
  -> `Int' throwGnutlsIfNeg* #}

{-
void        gnutls_certificate_set_rsa_export_params (gnutls_certificate_credentials_t res, gnutls_rsa_params_t rsa_params);
void        gnutls_certificate_set_verify_flags (gnutls_certificate_credentials_t res, unsigned int flags);
void        gnutls_certificate_set_verify_limits (gnutls_certificate_credentials_t res, unsigned int max_bits, unsigned int max_depth);

int         gnutls_certificate_set_x509_trust_mem (gnutls_certificate_credentials_t res, const gnutls_datum_t *CA, gnutls_x509_crt_fmt_t type);
int         gnutls_certificate_set_x509_trust (gnutls_certificate_credentials_t res, gnutls_x509_crt_t *ca_list, int ca_list_size);

int         gnutls_certificate_set_x509_crl_mem (gnutls_certificate_credentials_t res, const gnutls_datum_t *CRL, gnutls_x509_crt_fmt_t type);
int         gnutls_certificate_set_x509_crl (gnutls_certificate_credentials_t res, gnutls_x509_crl_t *crl_list, int crl_list_size);

int         gnutls_certificate_set_x509_key_mem (gnutls_certificate_credentials_t res, const gnutls_datum_t *CERT, const gnutls_datum_t *KEY, gnutls_x509_crt_fmt_t type);
int         gnutls_certificate_set_x509_key (gnutls_certificate_credentials_t res, gnutls_x509_crt_t *cert_list, int cert_list_size, gnutls_x509_privkey_t key);
-}

--------------------------------------------------------------------------------
-------------------------   DH and RSA Parameters   ----------------------------
--------------------------------------------------------------------------------

class CredParameter a where 
  -- | Generate a new key with the given number of bits.
  generate   :: a -> Int -> IO ()

newDH :: IO DH
newDH = alloca $ \ptr -> do
  gnutls_dh_params_init ptr >>= throwGnutlsIf
  raw<- peek ptr
  rc <- newRefCount (gnutls_dh_params_deinit raw)
  fp <- FC.newForeignPtr raw (freeRef rc)
  return $ DH fp rc

foreign import ccall safe "TLS.h gnutls_dh_params_deinit" gnutls_dh_params_deinit :: Ptr () -> IO ()
foreign import ccall safe "TLS.h gnutls_dh_params_init"   gnutls_dh_params_init :: Ptr (Ptr ()) -> IO CInt

instance CredParameter DH where
  generate a i = withDH a (\ap -> {#call gnutls_dh_params_generate2 #} ap (fromIntegral i) >>= throwGnutlsIf)

newRSA :: IO RSA
newRSA = alloca $ \ptr -> do
  gnutls_rsa_params_init ptr >>= throwGnutlsIf
  raw<- peek ptr
  rc <- newRefCount (gnutls_rsa_params_deinit raw)
  fp <- FC.newForeignPtr raw (freeRef rc)
  return $ RSA fp rc

foreign import ccall safe "TLS.h gnutls_rsa_params_deinit" gnutls_rsa_params_deinit :: Ptr () -> IO ()
foreign import ccall safe "TLS.h gnutls_rsa_params_init"   gnutls_rsa_params_init :: Ptr (Ptr ()) -> IO CInt

instance CredParameter RSA where
  generate a i = withRSA a (\ap -> {#call gnutls_rsa_params_generate2 #} ap (fromIntegral i) >>= throwGnutlsIf)


--------------------------------------------------------------------------------
---------------------------   Miscellaneous   ----------------------------------
--------------------------------------------------------------------------------

-- | Return the cipher's key size in bytes.
{#fun pure unsafe gnutls_cipher_get_key_size as cipherKeySize 
  {enumCInt `CipherAlgorithm'} -> `Int' fromIntegral #}

-- | Return the name of the ciphersuite.
{#fun pure unsafe gnutls_cipher_suite_get_name as cipherSuiteName 
  {enumCInt `KxAlgorithm',
   enumCInt `CipherAlgorithm',
   enumCInt `MacAlgorithm'} 
 -> `String' safePeekCString* #}

-- | The version of /Gnutls/ used.
version = unsafePerformIO $ {#call gnutls_check_version #} nullPtr >>= safePeekCString

{-# NOINLINE gnutlsGlobalInitIORef #-}
gnutlsGlobalInitIORef = unsafePerformIO $ do
  gcry_init_helper
  {#call unsafe gnutls_global_init #} >>= throwGnutlsIf
  newIORef ()

gnutlsGlobalInit = readIORef gnutlsGlobalInitIORef

foreign import ccall unsafe "gcry_init_helper" gcry_init_helper :: IO ()

