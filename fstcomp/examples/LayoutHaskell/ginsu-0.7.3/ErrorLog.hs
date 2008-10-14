--  $Id: ErrorLog.hs,v 1.1 2008-10-14 18:01:37 apel Exp $
-- arch-tag: 3849d358-d4bb-4bfc-b95f-a99a510cf553

-- Copyright (c) 2002 John Meacham (john@foo.net)
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


-- | Manages an error log with proper locking. has a number of useful routines for detecting
-- and reporting erronious conditions.

module ErrorLog(
    -- * Log handling
    LogLevel(..),
    withErrorLog,
    withStartEndEntrys,
    withErrorMessage,
    setLogLevel,
    setErrorLogPutStr,
    -- ** adding log entries
    putLogLn,putLog,
    putLogException,
    -- ** annotating exceptions
    emapM, eannM,
    -- ** exception-aware composition
    retry,
    first,
    tryMap, tryMapM,
    trySeveral,
    -- ** random functions
    attempt, tryElse, tryMost, tryMost_, catchMost,
    handleMost,
    ioElse,
    indent
    ) where

import Control.Exception as E
import IO hiding(bracket, try, catch)
import System.IO.Unsafe
import Monad
import Control.Concurrent
import Time(getClockTime)
import List(delete)

------------
-- Error log
------------

data LogLevel = LogEmergency | LogAlert | LogCritical | LogError | LogWarning | LogNotice | LogInfo | LogDebug
    deriving (Eq, Enum, Ord)

{-# NOINLINE ior #-}
ior :: MVar Handle
ior = unsafePerformIO $ newMVar stderr

{-# NOINLINE log_level #-}
log_level :: MVar LogLevel
log_level = unsafePerformIO $ newMVar LogNotice

{-# NOINLINE hPutStr_v #-}
hPutStr_v :: MVar (Handle -> String -> IO ())
hPutStr_v = unsafePerformIO $ newMVar hPutStr

-- | open file for logging and run action, with errors being logged to the file.
-- This will reinstall the old errorlog handle when it finishes, by default stderr
-- is used and this routine need not be called unless you wish to log somewhere else.
-- the filename consisting of a single dash is treated specially and sets the errorlog
-- to stderr. note, that while the errorlog will function properly with concurrent
-- applications, a single errorlog is shared by all threads.
withErrorLog :: String    -- ^ filename of log
		-> IO a      -- ^ action to execute with logging to file
		-> IO a
withErrorLog "-" action = bracket (swapMVar ior stderr) (swapMVar ior) (\_ -> action)
withErrorLog fn action = E.bracket (openFile fn WriteMode) hClose $ \h -> do
	hSetBuffering h LineBuffering
	bracket (swapMVar ior h) (swapMVar ior) (\_ -> action)

-- | sets log level to new value, returns old log level.
setLogLevel :: LogLevel -> IO LogLevel
setLogLevel ll = swapMVar log_level ll

-- | add entries to log at the start and end of action with timestamp.
-- If the action throws an exception, it will be logged along with the
-- exit entry.
withStartEndEntrys :: String  -- ^ title to use in log entries
		      -> IO a    -- ^ action to execute
		      -> IO a
withStartEndEntrys n action = do
    gct >>= \ct -> putLogLn (ct ++ " " ++ n ++ " Starting")
    handle
	(\e -> gct >>= \ct -> putLogException (ct ++ " " ++ n ++ " Ending due to Exception:" ) e >> throw e)
	(action >>= \r -> gct >>= \ct -> putLogLn (ct ++ " " ++ n ++ " Ending") >> return r) where
	    gct = getClockTime >>= \ct -> return $ "[" ++ show ct ++ "]"


-- | run an action, printing an error message to the log if it ends with an exception.
-- this is similar to 'withStartEndEntrys' but only adds an entry on error.
withErrorMessage :: String -> IO a -> IO a
withErrorMessage n action = do
    handleMost
	(\e -> gct >>= \ct -> putLogLn (normalize n ++ ct ++ " Ending due to Exception:\n" ++ indent 4 (show e) ) >> throw e )
	action  where
	    gct = getClockTime >>= \ct -> return $ "[" ++ show ct ++ "]"


-- | set routine with same signature as 'hPutStr' to use for writing to log.
-- useful for charset conversions which might be necisarry. By default the
-- haskell 98 'IO.hPutStr' is used.
setErrorLogPutStr :: (Handle -> String -> IO ()) -> IO ()
setErrorLogPutStr hp = swapMVar hPutStr_v hp >> return ()



normalize :: String -> String
normalize = unlines . lines

-- | place log entry, normalize string to always have a single \'\n\' at the end
-- of the string. A single log entry is created for each 'putLogLn', do not
-- split entrys among calls to this function.
putLogLn :: String -> IO ()
putLogLn s = do
    hp <- readMVar hPutStr_v
    withMVar ior (\h -> hp h (normalize s))
    withMVar ior (\h -> hFlush h)

{-
-- | log entry, depreciated. will be used for more general logging interface at some point.
putLog :: String -> IO ()
putLog s = do
    hp <- readMVar hPutStr_v
    withMVar ior (\h -> hp h s)

-}

-- | create log entry with given loglevel. entry is normalized as in 'putLogLn'.
putLog :: LogLevel -> String -> IO ()
putLog ll s = do
    cll <- readMVar log_level
    when (ll <= cll) $ putLogLn s

-- | transform an exception with a function.
emapM :: (Exception -> Exception) -> IO a -> IO a
emapM f action = do
    handle (\e -> throw (f e)) action


-- | annotates an exception using emapM, the original
-- type of the error cannot be recovered so this should only be used
-- if the exception is not meant to be caught later.
eannM :: String -> IO a -> IO a
eannM s action = emapM f action where
    f (ErrorCall es) = ErrorCall $ normalize s ++ normalize es
    f e = ErrorCall $ normalize s ++ normalize (show e)

-- | attempt an action, add a log entry with the exception if it
-- fails
attempt :: IO a -> IO ()
attempt action = tryMost action >>= \x -> case x of
    Left e -> putLogException "attempt ExceptionCaught" e
    Right _ -> return ()


tryElse r x = tryMost x >>= \y -> case y of
    Left e -> putLogException "tryElse ExceptionCaught" e >> return r
    Right v -> return v

tryMost_ x = tryMost x >> return ()

tryMap :: (a -> b) -> [a] -> IO [b]
tryMap f xs = do
    ys <- mapM (tryMost . evaluate . f ) xs
    return [y|(Right y) <- ys]

tryMapM :: (a -> IO b) -> [a] -> IO [b]
tryMapM f xs = do
    ys <- mapM (tryMost . f ) xs
    return [y|(Right y) <- ys]

tryMost = E.tryJust passKilled

passKilled (AsyncException ThreadKilled) = Nothing
passKilled x = Just x

catchMost = E.catchJust passKilled
handleMost = E.handleJust passKilled

-- | return the first non-excepting action. if all actions throw exceptions,
-- the last actions exception is rethrown.
first :: [IO a] -> IO a
first [] = fail "empty  argument to first"
first [x] = x
first (x:xs) = E.try x >>= \z -> case z of
    Left e@(AsyncException ThreadKilled) -> throw e
    Left _ -> first xs
    Right v -> return v

ioElse :: IO a -> IO a -> IO a
ioElse a b = tryMost a >>= \x -> case x of
    Left _ -> b
    Right x -> return x

indent :: Int -> String -> String
indent n s = unlines $ map (replicate n ' ' ++)$ lines s

-- | Retry an action untill it succeeds.
retry :: Float      -- ^ number of seconds to pause between trys
	 -> String  -- ^ string to annotate log entries with when retrying
	 -> IO a    -- ^ action to retry
	 -> IO a
retry delay n action = do
    handleMost (\e -> putLogException (n ++ " (retrying in " ++ show delay ++ "s):") e >> threadDelay (floor $ 1000000 * delay) >> retry delay n action) action


putLogException :: String -> Exception -> IO ()
putLogException n e =  putLog LogError (n ++ "\n" ++ indent 4 (show e))


-- | concurrently try several IO actions, returning the result of the first to finish.
-- if all actions throw exceptions, one is passed on non-deterministically
trySeveral :: [IO a] -> IO a
trySeveral [] = error "trySeveral has nothing to try!"
trySeveral arms = do
    v <- newEmptyMVar
    ts <- mapM (forkIO . f v) arms
    g v ts where
	f v arm = do
	    t <- myThreadId
	    r <- tryMost arm
	    putMVar v (t,r)
	g v ts = do
	    (t,r) <- takeMVar v
	    let ts' = delete t ts
	    case r of
		Left e -> if null ts' then throw e else g v ts'
		Right x -> do
		    mapM_ killThread ts'
		    return x
