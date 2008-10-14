-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module Server (main) where

import MimeTypes
import Request
import Response
import ErrorLogger
import AccessLogger
import ConfigParser
import Config hiding (listen)
import Global
import Util

import System
import Network.URI
import System.Posix
import Network.BSD
import IO hiding (bracket)
import Control.Exception as Exception
import List
import Maybe
import Monad
--import IOExts		hiding (trace)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Concurrent
import Network.Socket	hiding (accept)
import System.Console.GetOpt

-- removing dependency on preprocessor
-- #if 0

-- #ifdef WASHCGI
-- #if WASHCGI == 1
-- import RegisterCGI
-- #endif
-- #if WASHCGI == 2
-- import AutoCGI
-- #endif
-- #endif

-- #else

import Dispatch

-- #endif

import RunCGI

import Deploy
import InitialDeployments

{- -----------------------------------------------------------------------------
ToDo:

- MAJOR:

- deal with http version numbers
- timeouts (partly done)
- languages
- CGI scripts
- per-directory permissions (ala apache)
- directory indexing
- error logging levels
- virtual hosts, per-directory config options.
- languages (content-language, accept-language)
- multipart/byteranges

- MINOR:

- access logging (various bits left)
- implement "listen" (listen on additional ports)
- implement user & group setting
- log time to serve request
- terminate & restart signal (like Apache's SIGHUP)
- don't die if the new configuration file contains errors after a restart
- reading config file may block, unsafe if we receive another SIGHUP
- common up headers with same name (eg. accept).
- implement if-modified-since (need to parse time)

- when we get a request for http://foo.com/bar, where 'bar' is a
  directory and contains an index.html, we need to send back a
  redirect for http://foo.com/bar/ (i.e. add the final slash),
  otherwise relative links from index.html will be relative to
  http://foo.com/ instead of http://foo.com/bar/.  eg. look at
  http://www.haskell.org/happy/.

- MAYBE:

- throttling if too many open connections (config: MaxClients)

----------------------------------------------------------------------------- -}
-- command-line options

data CmdLineOpt
  = O_ConfigFile String
  | O_ServerRoot String

options = [
  Option ['f'] ["config"] (ReqArg O_ConfigFile "filename") 
	("default: " ++ show defaultConfigFile),
  Option ['d'] ["server-root"]  (ReqArg O_ServerRoot "directory")
	("default: " ++ show defaultServerRoot)
  ]

usage = "usage: hws [option...]"

cmdline :: IORef [CmdLineOpt]
cmdline = unsafePerformIO (newIORef (error "no flags"))

defaultConfigFile = "conf/httpd.conf"
defaultServerRoot = "/etc/httpd"

-- ToDo: set this in main
serverRoot = unsafePerformIO $ do
  args <- readIORef cmdline
  case [ s | O_ServerRoot s <- args] of
	[]    -> return defaultServerRoot
	(s:_) -> return s

configFile = unsafePerformIO $ do
  args <- readIORef cmdline
  case [ s | O_ConfigFile s <- args] of
	[]    -> return defaultConfigFile
	(s:_) -> return s
  
configPath = 
  case configFile of
    '.':'/':_ -> configFile
    '/':_ -> configFile
    _ -> serverRoot ++ '/':configFile

-----------------------------------------------------------------------------
-- Top-level server

main = do
  args <- getArgs
  case getOpt Permute options args of
     (flags, [], []) -> do 
	 writeIORef cmdline flags
	 main2
     (_, _, errs)    -> do 
	 hPutStr stderr (concat errs)
	 hPutStr stderr (usageInfo usage options)

main2 = do
  main_thread <- myThreadId
  installHandler sigPIPE Ignore Nothing
  installHandler sigHUP (Catch (hupHandler main_thread)) Nothing
  block $ readConfig

hupHandler :: ThreadId -> IO ()
hupHandler main_thread
  = throwTo main_thread (ErrorCall "**restart**")

sigsToBlock = addSignal sigHUP emptySignalSet

-- Async exceptions should be blocked on entry to readConfig (so that
-- multiple SIGHUPs close together can't kill us).  Make sure that
-- there aren't any interruptible operations until we've blocked signals.
readConfig = do
    blockSignals sigsToBlock
    r <- parseConfig configPath
    case r of
      Left err -> do
	hPutStrLn stderr ("failed to parse " ++ configPath)
	hPutStr stderr (show err)
      Right b  -> do
	let conf = b defaultConfig
	
	initMimeTypes (typesConfig conf) 	-- read mime.types
	startErrorLogger  conf			-- start the error log thread
  	startAccessLogger conf		 	-- start the logging thread
	  
	InitialDeployments.init			-- register builtin applications
	
  	my_hostent <- do			-- get my hostname/address
     	   ent <- getHostEntry
     	   case serverName conf of		-- Replace the name if
        	"" -> return ent		-- serverName is set.
		n  -> return ent{hostName = n}
  	putMVar local_hostent my_hostent

        topServer conf

-- We catch exceptions from the main server thread, and restart the
-- server.  If we receive a restart signal (from a SIGHUP), then we
-- re-read the configuration file.
topServer conf
  = Exception.catch
               (do unblockSignals sigsToBlock
		   unblock $ do
		   server conf)
	(\e -> case e of
		   ErrorCall "**restart**" -> do
			takeMVar local_hostent
			stopAccessLogger 
			stopErrorLogger
			readConfig
		   IOException io -> do
			logError ("server: " ++ showIOError io)
			topServer conf
		   _other -> do 
			logError ("server: " ++ show e)
			topServer conf
	)

-- open the server socket and start accepting connections
server conf = do
  proto <- getProtocolNumber "tcp"
  Exception.bracket
     (socket AF_INET Stream proto)
     (\sock -> sClose sock)
     (\sock -> do
        setSocketOption sock ReuseAddr 1
        bindSocket sock (SockAddrInet (fromIntegral (port conf)) iNADDR_ANY)
        listen sock maxListenQueue
	acceptConnections conf sock
    )

-- accept connections, and fork off a new thread to handle each one
acceptConnections conf sock = do
  (h, SockAddrInet port haddr) <- accept sock
  forkIO ( (talk conf h haddr  `finally`  (hClose h))
            `Exception.catch` 
          (\e -> trace ("servlet died: "  ++ show e) (return  ()))
	)
  acceptConnections conf sock


talk conf h haddr = do
  hSetBuffering h LineBuffering
  run conf True h haddr


run conf first h haddr = do
    -- read a request up to the first empty line.  If we
    -- don't get a request within the alloted time, issue
    -- a "Request Time-out" response and close the connection.
    let time_allowed | first     = requestTimeout conf
		     | otherwise = keepAliveTimeout conf

    trace "Reading request..." $ do
    req <- catchJust ioErrors (
	    timeout time_allowed
	      (do r <- getRequest h
		  return (Just r))
              (do -- only send a "request timed out" response if this
		  -- was the first request on the socket.  Subsequent
		  -- requests time-out and close the socket silently.
		  -- ToDo: if we get a partial request, still emit the
		  -- the timeout response.
		  (if first
			then response conf h (requestTimeOutResponse conf)
			else return ())
	          return Nothing)
	   )
#if __GLASGOW_HASKELL__ < 409 || __GLASGOW_HASKELL__ >= 600
	   (\e@io -> 
#else
	   (\e@(IOException io) -> 
#endif
		if isEOFError e
		     then trace "EOF from client" $ return Nothing
		     else do logError ("request: " ++ showIOError io) 
			     return Nothing )

    case req of { Nothing -> return ();  Just r ->
    trace "Got request" $ do

    -- tmp: dump out the request
#ifdef DEBUG
    mapM_ (hPutStrLn stderr) r
#endif

    case parseRequest r of

	 -- close the connection after a badly formatted request
	 Bad resp -> do 
	      trace ("Bad : "++show (resp conf)) $ do
	      response conf h (resp conf)
	      return ()

	 Ok  req  -> do 
	      resp <- request haddr conf req
	      trace ("Ok : "++show resp) $ do
	      logAccess req resp haddr (error "noTimeDiff"){-ToDo-}
	      response conf h resp

	      -- Persistent Connections
	      --
	      -- We close the connection if
	      --   (a) client specified "connection: close"
	      --   (b) client is pre-HTTP/1.1, and didn't
	      --       specify "connection: keep-alive"

	      let connection_headers = getConnection (reqHeaders req)
	      if ConnectionClose `elem` connection_headers
	         || (reqHTTPVer req < http1_1
		     && ConnectionKeepAlive `notElem` connection_headers)
		   then return ()
		   else run conf False h haddr
   }

-- Read lines up to the first empty line
-- ToDo: handle LWS
getRequest :: Handle -> IO [String]
getRequest h = do
  l <- hGetLine h
  if (emptyLine l) 
     then getRequest h
     else getRequest' l h

getRequest' l h = do
  if (emptyLine l) 
     then return []
     else do l' <- hGetLine h
	     ls <- getRequest' l' h
	     return (l:ls)

-----------------------------------------------------------------------------
-- Dealing with requests

request :: HostAddress -> Config -> Request -> IO Response
request haddr conf req@Request{reqCmd = cmd}
  = ( -- make sure we've got a host field 
      -- if the request version is >= HTTP/1.1
      trace "request" $
      case checkHostHeader conf req of
         Just response -> 
	   trace "checkHostHeader failed\n" $
	   return (response conf)
	 Nothing -> case cmd of
	              GetReq  -> doGetPost haddr conf req False
		      HeadReq -> doGetPost haddr conf req True
		      PostReq -> doGetPost haddr conf req False
		      _ -> return (notImplementedResponse conf)
    ) 
      `Exception.catch`
    ( \exception -> do
	 logError ("request: " ++ show exception)
         return (internalServerErrorResponse conf)
    )

checkHostHeader conf Request{reqHTTPVer = ver, reqHeaders = headers}
  = case getHost headers of
	   [] | ver < http1_1			 -> Nothing
	   [host] | host == serverName conf 
		  || host `elem` serverAlias conf -> Nothing
		  | otherwise			 -> Just notFoundResponse
	   _					 -> Just badRequestResponse

doGetPost haddr conf Request{reqURI = uri, reqHeaders = headers, reqCmd = cmd} is_head
 = trace ("doGetPost: "++show uri) $ case uri of
      NoURI          -> return (badRequestResponse conf)
      AuthorityURI _ -> return (badRequestResponse conf)
      AbsPath path   -> getPath haddr conf cmd path is_head headers
      AbsURI uri     -> getURI  haddr conf cmd uri  is_head headers

-- | handles the proxy case (why? it should never happen to an origin server.)
getURI haddr conf cmd uri is_head headers =
#if 1
  return (notAcceptableResponse conf)
#else
  let
    scheme = uriScheme uri
    mauth  = uriAuthority uri
    path   = unEscapeString $ uriPath uri
    query  = unEscapeString $ uriQuery uri
    frag   = unEscapeString $ uriFragment uri
  in
  case scriptAlias conf of
    Just (scriptPrefix, mappedPath) 
      | scheme == "http:" && scriptPrefix `isPrefixOf` path ->
	getScript scriptPrefix haddr conf path [query]
	          (requestCmdString cmd) is_head headers
    _ ->
      if (scheme /= "http:")
	 || (case mauth of
	      Nothing -> True
	      Just auth -> uriRegName auth /= host)
	 || (query /= "")
	 || (frag /= "")
	 || cmd == PostReq
	 then return (notFoundResponse conf) 
	 else getFile conf path is_head headers
#endif

-- | handles the common case where where the request is in terms of an absolute
-- path. 
getPath haddr conf cmd path is_head headers = 
  let reqString = requestCmdString cmd 
      args = []
  in
  do dispatched <- runDispatch conf path 
     case dispatched of
       Nothing | cmd == PostReq ->
	 return (notFoundResponse conf)
       Nothing ->
	 getFile conf path is_head headers
       Just (pathinfo, options, pgm) -> do 
	 host <- inet_ntoa haddr
	 return (okResponse conf
   	     (RunMe (\h ->
  	      runCGI h host conf path pathinfo args reqString is_head headers options pgm))
   	      [{-headers-}]
   	      (not is_head) {- send body -})
	    
getFile conf path is_head headers = 
   trace ("getFile " ++ path) $ do
   m_path <- prependDocRoot conf path
   case m_path of {
	Left r -> return r;
	Right path -> do

   check <- findRealFilename conf path headers
   trace ("reallyGetFile " ++ show (either (Left . id) (Right . fst) check)) $
     case check of { 
       Left r -> return r;
       Right (filename,stat) -> do
   
   	   -- check we can actually read this file
   access <- fileAccess filename True{-read-} False False
   case access of {
       False -> return (notFoundResponse conf);
   		   -- not "permission denied", we're being paranoid
   		   -- about security.
       True -> do
   
   let content_type = 
   	case mimeTypeOf filename of
   		 Nothing -> contentTypeHeader (show (defaultType conf))
   		 Just t  -> contentTypeHeader (show t)
   
   let last_modified = 
	 lastModifiedHeader (epochTimeToClockTime (modificationTime stat))

   let size = toInteger (fileSize stat)
   
   trace ("getFile " ++ show (mimeTypeOf filename)) $
     return (okResponse conf
   	      (FileBody size filename)
   	      [content_type, last_modified]
   	      (not is_head) {- send body -})
   }}}

-- expand "~user" in the path name
prependDocRoot :: Config -> String -> IO (Either Response String)
prependDocRoot conf ('/':'~':userpath) | not (null (userDir conf)) = do
  let (user, path) = break (=='/') userpath
  u_ent <- tryJust ioErrors (getUserEntryForName user)
  case u_ent of
    Left _    -> return (Left (notFoundResponse conf))
    Right ent -> return (Right (homeDirectory ent ++ 
				'/':userDir conf ++ path))
prependDocRoot conf path@('/':_) = do
  return (Right (documentRoot conf ++ path))
prependDocRoot conf _path = return (Left (notFoundResponse conf))

findRealFilename :: Config -> String -> [RequestHeader] -> IO (Either Response (String,FileStatus))
findRealFilename conf filename headers = do
  stat <- statFile filename
  case stat of
      Nothing -> 
         tryMultiViews conf filename headers
      Just stat
	 | isDirectory stat -> do
	     let index_filename = filename ++ '/': directoryIndex conf
	     stat <- statFile index_filename
	     case stat of
	         Nothing -> return (Left (notFoundResponse conf))
		 Just stat -> return (Right (index_filename,stat))
         | isRegularFile stat ->
	     return (Right (filename,stat))
	 | otherwise ->
	     return (Left (notFoundResponse conf))

-- experimental: should really map language names to extensions separately in
-- a configurable way
tryMultiViews conf filename hdrs =
    let acceptableLanguages = 
	  (unTangle $ getAcceptLanguage hdrs) ++ languagePriority conf in
    g acceptableLanguages
  where
    g [] = return (Left (notFoundResponse conf))
    g (code:codes) = 
      case lookup code (addLanguage conf) of
      Nothing -> g codes
      Just ext -> 
        let newFilename = filename ++ ext in
	do mstat <- statFile newFilename
	   case mstat of
	     Nothing -> 
	       case break (=='-') code of
	         (_,"") -> g codes
		 (codeprefix, _) -> g (codeprefix:codes)
	     Just stat -> return (Right (newFilename,stat))
    unTangle [] = []
    unTangle ("":langs) = unTangle langs
    unTangle (lang:langs) = 
      case break (==',') lang of
        (code, "") -> code: unTangle langs
	(code, _:rest) -> code: unTangle (rest:langs)
      


-- ignore port for now
getHost hdrs = [ h | Host h p <- hdrs ]
getConnection hdrs = [ c | Connection cs <- hdrs, c <- cs ]
getAcceptLanguage hdrs = [ al | AcceptLanguage al <- hdrs ]

-----------------------------------------------------------------------------
-- older GHC compat

#if __GLASGOW_HASKELL__ < 409
catchJust = Exception.catchIO
tryJust   = Exception.tryIO
ioErrors = justIoErrors
#endif
