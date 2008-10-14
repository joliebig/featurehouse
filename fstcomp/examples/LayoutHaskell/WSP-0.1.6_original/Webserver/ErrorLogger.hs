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

module ErrorLogger (
	startErrorLogger, 
	stopErrorLogger, 
	logError, 
	catchAndLogError,
	showIOError
  ) where

import Config
import Util

import Time
import IO
import System.IO.Unsafe
import Control.Concurrent
import Control.Exception as Exception

#if __GLASGOW_HASKELL__ < 409
import PrelIOBase (IOError(..))
#elif __GLASGOW_HASKELL__ < 504
import PrelIOBase (IOException(..))
#else
import GHC.IOBase (IOException(..))
#endif

-----------------------------------------------------------------------------
-- Error Logging

logError :: String -> IO ()
logError err
  = writeChan error_log_chan err
    
error_log_chan :: Chan String
error_log_chan = unsafePerformIO (newChan)

error_log_pid :: MVar ThreadId
error_log_pid = unsafePerformIO (newEmptyMVar)

startErrorLogger :: Config -> IO ()
startErrorLogger conf = do
  logError ("error logger started (level " ++ show (logLevel conf) ++  
	    ") on '" ++ errorLogFile conf ++ "'")
  t <- forkIO (Exception.catch (run_error_logger conf) (error_handler conf))
  putMVar error_log_pid t

stopErrorLogger :: IO ()
stopErrorLogger = do
   t <- takeMVar error_log_pid
   throwTo t (ErrorCall "**stop**")

error_handler conf (ErrorCall "**stop**") =
   logError ("error logger stopped")
error_handler conf exception = do
   logError ("error logger died: " ++ show exception)
   Exception.catch (run_error_logger conf) (error_handler conf)

run_error_logger conf = do
   Exception.bracket 
      (openFile (errorLogFile conf) AppendMode) 
      (\hdl -> hClose hdl)
      (\hdl -> doErrLogRequests hdl)

doErrLogRequests hdl = do
  str <- readChan error_log_chan
  clock_time <- getClockTime
  let time_str = formatTimeSensibly (toUTCTime clock_time)
  hPutStr hdl time_str
  hPutStrLn hdl ("  " ++ str)
  hFlush hdl
  doErrLogRequests hdl

catchAndLogError :: String -> IO a -> (Exception -> IO a) -> IO a
catchAndLogError str io handler 
  = Exception.catch io (\e -> logError (str ++ show e) >> handler e)

#if __GLASGOW_HASKELL__ < 409
showIOError :: IOError -> String
#else
showIOError :: IOException -> String
#endif
showIOError (IOError _hdl iot loc s m_filename)
  = ( showString loc
    . showString ": "
    . shows iot
    . showString " ("
    . showString s
    . showString ") in ["
    . shows m_filename
    . showChar ']'
  ) ""
