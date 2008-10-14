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

module AccessLogger (
	startAccessLogger,
	stopAccessLogger,
	logAccess
  ) where

import ErrorLogger
import Request
import Response
import Config
import Global
import Util

import IO
import Char (toLower)
--import IOExts hiding (trace)
import System.IO.Unsafe
import System.Time
--import SocketPrim
import Network.Socket
import Network.BSD
import Control.Exception as Exception
import Control.Concurrent

-----------------------------------------------------------------------------
-- Access Logging

-- logging is done by a separate thread, so it doesn't hold anything else up.

data LogRequest =
  LogReq { 
     log_ipaddr	        :: HostAddress,		-- %a
     log_logname	:: String,		-- %l
     log_request	:: Request,		-- %r etc.
     log_response	:: Response,		-- 
     log_time	        :: ClockTime,		-- %{format}t
     log_delay		:: TimeDiff,		-- %T
     log_user	        :: String,		-- %u
     log_server	        :: HostEntry		-- %v
  }

logAccess :: Request -> Response -> HostAddress -> TimeDiff -> IO ()
logAccess req resp haddr delay
  = do
    time <- getClockTime

    -- ToDo: for servers serving multiple virtual domains, this
    -- servername should be dynamic.
    server <- readMVar local_hostent

    writeChan access_log_chan
      LogReq {
        log_ipaddr = haddr,
        log_logname = "<nobody>", -- ToDo
        log_request = req,
        log_response = resp,
        log_time = time,
        log_delay = delay,
        log_user = "<nouser>", -- ToDo
	log_server = server
      }
 where
    
access_log_chan :: Chan LogRequest
access_log_chan = unsafePerformIO (newChan)

access_log_pid :: MVar ThreadId
access_log_pid = unsafePerformIO (newEmptyMVar)

startAccessLogger :: Config -> IO ()
startAccessLogger conf = do
  logError ("access logger started on '" ++ accessLogFile conf ++ "'")
  t <- forkIO (Exception.catch (run_access_logger conf) (error_handler conf))
  putMVar access_log_pid t

-- ToDo: shouldn't really kill the access logger with a signal, it might
-- be partway through servicing a log request.
stopAccessLogger :: IO ()
stopAccessLogger = do
   t <- takeMVar access_log_pid
   throwTo t (ErrorCall "**stop**")

error_handler conf (ErrorCall "**stop**") =
   logError ("access logger stopped")
error_handler conf exception = do
   logError ("access logger died: " ++ show exception)
   Exception.catch (run_access_logger conf) (error_handler conf)

run_access_logger conf =
   Exception.bracket 
      (openFile (accessLogFile conf) AppendMode) 
      (\hdl -> hClose hdl)
      (\hdl -> doLogRequests conf hdl)

doLogRequests conf hdl = do
  req <- readChan access_log_chan
  ip_addr <- inet_ntoa (log_ipaddr req)
  -- look up the hostname if hostnameLookups is on
  host <- if hostnameLookups conf
	     then do catchJust ioErrors
			  (do ent <- getHostByAddr AF_INET (log_ipaddr req)
			      return (Just ent)
			  )
			  (\_ -> return Nothing)
	     else return Nothing
  let line = mkLogLine req ip_addr host (accessLogFormat conf)
  hPutStrLn hdl line
  hFlush hdl
  doLogRequests conf hdl

-- ToDo: could probably make this a lot faster by pre-parsing the log
-- specification.

mkLogLine
	:: LogRequest			-- info to log
	-> String			-- IP addr if we need it
	-> Maybe HostEntry		-- hostname
	-> String			-- log format
	-> String

mkLogLine _info _ip_addr _host "" = ""
mkLogLine info ip_addr host ('%':'{':rest)
  = expand info ip_addr host (Just str) c ++ mkLogLine info ip_addr host rest1
  where (str, '}':c:rest1) = span (/= '}') rest
mkLogLine info ip_addr host ('%':c:rest)
  = expand info ip_addr host Nothing c ++ mkLogLine info ip_addr host rest
mkLogLine info ip_addr host (c:rest) = c : mkLogLine info ip_addr host rest


expand info ip_addr host arg c = 
          case c of
            'b'	-> show (contentLength  resp_body)
	    'f'	-> getFileName resp_body

	    -- %h is the hostname if hostnameLookups is on, otherwise the 
	    -- IP address.
	    'h' -> case host of
			Just ent -> hostName ent
			Nothing  -> ip_addr
	    'a' -> ip_addr
	    'l' -> log_logname info
	    'r' -> show (log_request info)
	    -- ToDo: 'p' -> canonical port number of server
	    's' -> show resp_code
	    't' -> formatTimeSensibly (toUTCTime (log_time info))
	    'T' -> timeDiffToString (log_delay info)
	    'v' -> hostName (log_server info)
	    'u' -> log_user info

	    'i' -> getReqHeader arg (reqHeaders (log_request info))
	    -- 'o' -> getRespHeader arg resp_headers

	    -- ToDo: other stuff
	    _ -> ['%',c]
  where
   Response {
    respCode = resp_code,
    respHeaders = resp_headers,
    respCoding = resp_coding,
    respBody = resp_body,
    respSendBody = resp_send_body
    } = log_response info

getReqHeader Nothing    _hdrs = ""
getReqHeader (Just hdr) hdrs = concat (
  case map toLower hdr of
    -- missing:
    -- Connection          [Connection]
    -- Date 		   String
    -- Pragma              String
    -- Trailer             String
    -- TransferEncoding    String
    -- Upgrade             String
    -- Via                 String
    -- Warning             String
    "accept"              -> [ s | Accept s <- hdrs ]
    "accept-charset"      -> [ s | AcceptCharset s <- hdrs ]
    "accept-encoding"     -> [ s | AcceptEncoding s <- hdrs ]
    "accept-language"     -> [ s | AcceptLanguage s <- hdrs ]
    "authorization"       -> [ s | Authorization s <- hdrs ]
    "cachecontrol"        -> [ s | CacheControl s <- hdrs ]
    -- Expect Expect
    "from"                -> [ s | From s <- hdrs ]
    -- Host String{-hostname-} (Maybe Int){-port-}
    "if-match"            -> [ s | IfMatch s <- hdrs ]
    "if-modified-since"   -> [ s | IfModifiedSince s <- hdrs ]
    "if-none-match"       -> [ s | IfNoneMatch s <- hdrs ]
    "if-range"            -> [ s | IfRange s <- hdrs ]
    "if-unmodified-since" -> [ s | IfUnmodifiedSince s <- hdrs ]
    "max-forwards"        -> [ s | MaxForwards s <- hdrs ]
    "proxy-authorization" -> [ s | ProxyAuthorization s <- hdrs ]
    "range"               -> [ s | Range s <- hdrs ]
    "referer"             -> [ s | Referer s <- hdrs ]
    "te"                  -> [ s | TE s <- hdrs ]
    "user-agent"          -> [ s | UserAgent s <- hdrs ]
    _                     -> []
   )

-----------------------------------------------------------------------------
-- older GHC compat

#if __GLASGOW_HASKELL__ < 409
catchJust = Exception.catchIO
ioErrors = justIoErrors
#endif
