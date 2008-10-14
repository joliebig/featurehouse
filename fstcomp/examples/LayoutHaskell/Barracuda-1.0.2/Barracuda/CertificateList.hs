-- |
-- Maintainer: Stephan Friedrichs, Henning Guenther
--
-- This module contains a list for the known certificates of internal and external users.
-- It also contains functions to cache the certificates on the hard disk.
module Barracuda.CertificateList (
	CertificateList,
	certificates,
	dumpCertificateList,
	loadCertificateList,
	verify
) where

import Control.Exception
import Control.Monad
import Data.ByteString (readFile,writeFile)
import Data.Map as Map
import Data.Maybe(catMaybes)
import Network.GnuTLS hiding (verifySignature)
import Network.GnuTLS.X509 hiding (verifySignature)
import Prelude hiding (writeFile,readFile)
import System.Directory
import System.FilePath
import Text.Regex

import Network.AdHoc.Signature
import Network.AdHoc.UserID

-- | Stores the 'Certificate' of every user in the network.
type CertificateList = Map UserID Certificate

certRegex :: Regex
certRegex = mkRegex "(.*)\\.cert"

isCertificate :: FilePath -> Maybe String
isCertificate path = do
	[user] <- matchRegex certRegex path
	return user

-- | Returns a complete list of 'UserID's and their 'Certificate's.
certificates :: CertificateList -> [(UserID, Certificate)]
certificates = assocs

genPath :: FilePath -> UserID -> FilePath
genPath path user = path </> userHost user </> userName user ++ ".cert"

writeCertificate :: FilePath -> UserID -> Certificate -> IO ()
writeCertificate path user cert = do
	let check_dir = path </> userHost user
	dirExists <- doesDirectoryExist check_dir
	unless dirExists (createDirectory check_dir)
	let outp = either (error.show) id (exportCertificate cert X509FmtPem)
	writeFile (genPath path user) outp

readCertificate :: FilePath -> String -> IO (Maybe (String,Certificate))
readCertificate path file = case isCertificate file of
	Just user -> do
		str <- readFile (path </> file)
		either (error.show) (\cert -> return $ Just (user,cert)) (importCertificate str X509FmtPem)
	Nothing -> return Nothing

verify :: CertificateList -> String -> Signature -> UserID -> SignatureStatus
verify cl str sig user = case Map.lookup user cl of
	Nothing -> CertificateMissing (verifySignature str sig)
	Just cert -> verifySignature str sig cert

-- | Writes the certificate list into a directory(dir). It uses the following schema:
--   dir\/host\/user.cert
dumpCertificateList :: CertificateList -> FilePath -> IO ()
dumpCertificateList cl path = do
	exists <- doesDirectoryExist path
	createDirectoryIfMissing True path
	conts <- getDirectoryContents path
	mapM_ (\dir -> unless (dir=="."||dir=="..") (removeDirectoryRecursive (path </> dir))) conts
	mapM_ (uncurry $ writeCertificate path) (certificates cl)

-- | Loads the certificate list from a given location using the same schema as 'dumpCertificateList'.
loadCertificateList :: FilePath -> IO CertificateList
loadCertificateList path = do
	exists <- doesDirectoryExist path
	if exists
		then do
			conts <- getDirectoryContents path
			certs <- mapM (\host -> do
				conts' <- getDirectoryContents (path </> host)
				mapM (\user -> do
					cert <- readCertificate (path </> host) user
					return $ maybe Nothing (\(ruser,rcert) -> Just (UserID ruser host,rcert)) cert
					) (drop 2 conts')
				) (drop 2 conts)
			return $ fromList $ catMaybes $concat certs
		else return Map.empty
