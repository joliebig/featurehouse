-- |
-- Maintainer: Henning Guenther
--
-- This module contains a gui widget used to load certificates and matching private keys.
module Barracuda.GUI.CertificateLoader
	(CertificateLoader
	,PrivateKeyLoader
	,Loader
	,certificateLoaderNew
	,privateKeyLoaderNew
	,loaderNew
	,certificateLoaderGet
	,privateKeyLoaderGet
	,certificateKeyMatch 
	,loaderGetWidget
	,certificateGetUserID
	) where

import Barracuda.Utils
import Control.Monad.Fix (mfix)
import Data.IORef
import Graphics.UI.Gtk
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import System.Directory (doesFileExist)
import Network.GnuTLS (importCertificate,importPrivateKey,getDnByOid
	,oidPKCS9Email,X509CertificateFormat(..),Certificate
	,PrivateKey,certificateGetKeyId,privateKeyGetKeyId)
import Network.AdHoc.UserID

-- | Loader widget for certificates.
data CertificateLoader = CertLoader FileChooserButton (IORef (Maybe String -> IO ()))

-- | Loader widget for private keys.
data PrivateKeyLoader  = PrivLoader FileChooserButton (IORef (IO ()))

data LoaderStatus = LoadStat HBox Image Label

-- | A loader widget including a 'CertificateLoader' and a 'PrivateKeyLoader'.
data Loader = Loader Table (IORef (Certificate -> PrivateKey -> IO ()))

contentFilterNew :: String -> (BS.ByteString -> Bool) -> IO FileFilter
contentFilterNew name f = do
	ff <- fileFilterNew
	fileFilterSetName ff name
	fileFilterAddCustom ff [toEnum 1] (\(Just fn) _ _ _ -> do	-- XXX: This is just because of a missing export in Gtk2Hs... shame on them
		str <- BS.readFile fn
		return $ f str)
	return ff

certificateFilterNew :: IO FileFilter
certificateFilterNew = contentFilterNew "PEM encoded certificates"
	(\str -> either (const False) (const True) $ importCertificate str X509FmtPem)

privateKeyFilterNew :: IO FileFilter
privateKeyFilterNew = contentFilterNew "PEM encoded private keys"
	(\str -> either (const False) (const True) $ importPrivateKey str X509FmtPem)

-- | Creates a 'FileChooserDialog' that previews the username of a selected
--   certificate.
certificateLoaderNew :: Maybe FilePath -- ^ An optional initial file selection
	-> (Maybe String -> IO ())     -- ^ Callback for user selection
	-> IO CertificateLoader
certificateLoaderNew certificate callback = mfix $ \result -> do
	dialog <- fileChooserButtonNew "Select certificate" FileChooserActionOpen
	props <- hBoxNew False 0
	lblFor <- labelNew (Just "<b>Username:</b> ")
	lblFor `set` [labelUseMarkup := True]
	txtFor <- labelNew Nothing
	txtFor `set` [labelWidthChars := 20,miscXalign := 0]
	boxPackStart props lblFor PackNatural 0
	boxPackStart props txtFor PackGrow 0
	fileChooserSetUsePreviewLabel dialog False
	fileChooserSetPreviewWidget dialog props
	fileChooserSetPreviewWidgetActive dialog True
	cbref <- newIORef callback
	dialog `onUpdatePreview` (do
		callback <- readIORef cbref
		cert <- certificateLoaderGet result
		let reset = txtFor `labelSetText` "" >> callback Nothing
		case cert of
			Nothing -> reset
			Just cert -> do
				case getDnByOid cert oidPKCS9Email 0 of
					Right (Just email) -> txtFor `labelSetText` (unpack email) >> callback (Just $ unpack email)
					_ -> reset
				files <- fileChooserGetFilenames dialog
				case files of
					(path:_) -> catch (getLastCertificate >>= (\cert -> writeFile cert path)) (const $ return ())
					_        -> return ()
		)
	ff <- certificateFilterNew
	fileChooserSetFilter dialog ff
	widgetShowAll dialog
	case certificate of
		Just path -> fileChooserSelectFilename dialog path >> return ()
		Nothing   -> return ()
	return (CertLoader dialog cbref)

-- | Tries to read the 'UserID' belonging to a 'Certificate'.
certificateGetUserID :: Certificate -> Maybe UserID
certificateGetUserID cert = case getDnByOid cert oidPKCS9Email 0 of
	Right (Just email) -> let 
		(name,host) = break (=='@') (unpack email)
		in if null host then Nothing else Just (UserID name (tail host))
	_ -> Nothing

-- | Creates a new 'PrivateKeyLoader'.
privateKeyLoaderNew :: Maybe FilePath -- ^ An optional initial selection
	-> IO ()                      -- ^ Callback for user selection
	-> IO PrivateKeyLoader
privateKeyLoaderNew key callback = do
	but <- fileChooserButtonNew "Select private key" FileChooserActionOpen
	ff <- privateKeyFilterNew
	fileChooserSetFilter but ff
	cbref <- newIORef callback
	but `onUpdatePreview` (do
		f <- readIORef cbref
		f
		files <- fileChooserGetFilename but
		case files of
			Just path -> catch (getLastKey >>= (\key -> writeFile key path)) (const $ return ())
			_         -> return ()
		)
	case key of
		Just path -> fileChooserSelectFilename but path >> return ()
		Nothing   -> return ()
	return (PrivLoader but cbref)

-- | Returns the currently selected 'Certificate'.
certificateLoaderGet :: CertificateLoader -> IO (Maybe Certificate)
certificateLoaderGet (CertLoader ct _) = do
	fn <- fileChooserGetFilename ct
	case fn of
		Nothing  -> return Nothing
		Just rfn -> loadCertificateFile rfn

loadCertificateFile :: FilePath -> IO (Maybe Certificate)
loadCertificateFile path = do
	exists <- doesFileExist path
	if exists
		then do
			str <- BS.readFile path
			case importCertificate str X509FmtPem of
				Left _     -> return Nothing
				Right cert -> return (Just cert)
		else return Nothing

-- | Returns the currently selected 'PrivateKey'.
privateKeyLoaderGet :: PrivateKeyLoader -> IO (Maybe PrivateKey)
privateKeyLoaderGet (PrivLoader ld _) = do
	fn <- fileChooserGetFilename ld
	case fn of
		Nothing -> return Nothing
		Just rfn -> loadPrivateKeyFile rfn

loadPrivateKeyFile :: FilePath -> IO (Maybe PrivateKey)
loadPrivateKeyFile path = do
	exists <- doesFileExist path
	if exists
		then do
			str <- BS.readFile path
			case importPrivateKey str X509FmtPem of
				Left _    -> return Nothing
				Right key -> return (Just key)
		else return Nothing

-- | Tries to match a 'Certificate' against a 'PrivateKey'.
certificateKeyMatch :: Certificate -> PrivateKey -> Maybe Bool
certificateKeyMatch cert key = case (do
	keyid_cert <- certificateGetKeyId cert
	keyid_key <- privateKeyGetKeyId key
	return $ keyid_cert == keyid_key
	) of
		Left err -> Nothing
		Right res -> Just res

loaderStatusNew :: IO LoaderStatus
loaderStatusNew = do
	box <- hBoxNew False 0
	img <- imageNew
	lbl <- labelNew Nothing
	lbl `set` [miscXalign:=0]
	boxPackStart box img PackNatural 0
	boxPackStart box lbl PackGrow 0
	return (LoadStat box img lbl)

loaderStatusSetAllMissing :: LoaderStatus -> IO ()
loaderStatusSetAllMissing (LoadStat _ img lbl) = do
	imageSetFromStock img stockDialogWarning iconSizeDialog
	lbl `set` [labelText:="No certificate and private key set."]

loaderStatusSetCertMissing :: LoaderStatus -> IO ()
loaderStatusSetCertMissing (LoadStat _ img lbl) = do
	imageSetFromStock img stockDialogWarning iconSizeDialog
	lbl `set` [labelText:="No certificate set."]

loaderStatusSetPrivMissing :: LoaderStatus -> IO ()
loaderStatusSetPrivMissing (LoadStat _ img lbl) = do
	imageSetFromStock img stockDialogWarning iconSizeDialog
	lbl `set` [labelText:="No private key set."]

loaderStatusSetNotMatch :: LoaderStatus -> IO ()
loaderStatusSetNotMatch (LoadStat _ img lbl) = do
	imageSetFromStock img stockDialogError iconSizeDialog
	lbl `set` [labelText:="Certificate and private key do not match."]

loaderStatusSetOk :: LoaderStatus -> IO ()
loaderStatusSetOk (LoadStat _ img lbl) = do
	imageSetFromStock img stockDialogInfo iconSizeDialog
	lbl `set` [labelText:="Okay."]

loaderStatusUpdate :: LoaderStatus -> CertificateLoader -> PrivateKeyLoader -> IO Bool
loaderStatusUpdate st cl pl = do
	mb_cert <- certificateLoaderGet cl
	mb_priv <- privateKeyLoaderGet pl
	loaderStatusUpdate' st mb_cert mb_priv

loaderStatusUpdate' :: LoaderStatus -> Maybe Certificate -> Maybe PrivateKey -> IO Bool
loaderStatusUpdate' st mb_cert mb_priv = do
	case mb_cert of
		Nothing -> (case mb_priv of
			Nothing -> loaderStatusSetAllMissing st 
			Just _  -> loaderStatusSetCertMissing st) >> return False
		Just cert -> case mb_priv of
			Nothing  -> loaderStatusSetPrivMissing st >> return False
			Just key -> case certificateKeyMatch cert key of
				Just False -> loaderStatusSetNotMatch st >> return False
				_ -> loaderStatusSetOk st >> return True

-- | Creates a new 'Loader'. It uses the default paths to read previously stored
--   selection paths to provide an initial configuration.
loaderNew :: (Certificate -> PrivateKey -> IO ()) -- ^ Callback for a new user selection
	-> IO Loader
loaderNew callback = do
	cert <- loadLastCertPath
	key  <- loadLastKeyPath
	loaderNew' cert key callback

loaderNew' :: Maybe FilePath -> Maybe FilePath -> (Certificate -> PrivateKey -> IO ()) -> IO Loader
loaderNew' cert key callback = mdo
	tab <- tableNew 8 2 False
	catCert <- labelNew (Just "<b>Certificate</b>")
	catCert `set` [labelUseMarkup := True,miscXalign:=0]
	descrCert <- labelNew (Just "Certificate file:")
	descrCert `set` [miscXalign:=0]
	descrUser <- labelNew (Just "Username:")
	descrUser `set` [miscXalign:=0]
	fieldUser <- labelNew Nothing
	fieldUser `set` [miscXalign:=0]
	cbref <- newIORef callback
	let updateBut = loaderStatusUpdate st cl pl >>= widgetSetSensitivity butOk
	st@(LoadStat box _ _) <- loaderStatusNew
	pl@(PrivLoader privLoader plcbref) <- privateKeyLoaderNew key updateBut
	cl@(CertLoader certLoader clcbref) <- certificateLoaderNew cert (\user -> do
		case user of
			Nothing -> fieldUser `set` [labelText := "<no username>"]
			Just email -> fieldUser `set` [labelText := email]
		updateBut)
	butOk <- buttonNewFromStock stockOk
	butOk `widgetSetSensitivity` False
	butOk `onClicked` (do
		-- Guaranteed to be Just, because otherwise the button wouldn't be pressable
		Just cert <- certificateLoaderGet cl
		Just key  <- privateKeyLoaderGet pl
		f <- readIORef cbref
		f cert key
		)
	loaderStatusUpdate st cl pl
	catPrivkey <- labelNew (Just "<b>Private key</b>")
	catPrivkey `set` [labelUseMarkup:=True,miscXalign:=0]
	descrPriv <- labelNew (Just "Private key file:")
	descrPriv `set` [miscXalign:=0]
	sep <- hSeparatorNew
	butBox <- vButtonBoxNew
	butBox `set` [buttonBoxLayoutStyle:=ButtonboxEnd
		     ,boxHomogeneous:=False]
	let loader = Loader tab cbref
	certificate <- case cert of
		Nothing -> return Nothing
		Just c  -> loadCertificateFile c
	privateKey <- case key of
		Nothing -> return Nothing
		Just k  -> loadPrivateKeyFile k
	loaderStatusUpdate' st certificate privateKey >>= widgetSetSensitivity butOk
	boxPackStart butBox butOk PackNatural 0
	tableAttach tab catCert 0 2 0 1 [Expand,Fill] [Expand,Fill] 0 0
	tableAttach tab descrCert 0 1 1 2 [Fill] [Expand,Fill] 10 0
	tableAttach tab certLoader 1 2 1 2 [Expand,Fill] [Expand,Fill] 0 0
	tableAttach tab descrUser 0 1 2 3 [Fill] [Expand,Fill] 10 0
	tableAttach tab fieldUser 1 2 2 3 [Expand,Fill] [Expand,Fill] 0 0
	tableAttach tab catPrivkey 0 2 3 4 [Expand,Fill] [Expand,Fill] 0 0
	tableAttach tab descrPriv 0 1 4 5 [Fill] [Expand,Fill] 10 0
	tableAttach tab privLoader 1 2 4 5 [Expand,Fill] [Expand,Fill] 0 0
	tableAttach tab sep 0 2 5 6 [Expand,Fill] [] 0 5
	tableAttach tab box 0 2 6 7 [Expand,Fill] [Expand,Fill] 0 0
	tableAttach tab butBox 0 2 7 8 [Expand,Fill] [Expand,Fill] 0 0
	return loader

-- | Returns the 'Widget' to be drawn.
loaderGetWidget :: Loader -> Widget
loaderGetWidget (Loader tab _) = toWidget tab

loaderOnEnter :: Loader -> (Certificate -> PrivateKey -> IO ()) -> IO ()
loaderOnEnter (Loader _ cb) f = writeIORef cb f

loadLastCertPath :: IO (Maybe String)
loadLastCertPath = catch (do
		path <- getLastCertificate
		cert <- readFile path
		return $ Just cert
	) (const (return Nothing))

loadLastKeyPath :: IO (Maybe String)
loadLastKeyPath = catch (do
		path <- getLastKey
		key <- readFile path
		return $ Just key
	) (const (return Nothing))

