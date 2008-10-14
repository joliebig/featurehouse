module Global where

import System.IO.Unsafe

import Network.BSD
import Control.Concurrent

-- | Local hostname.  We'll update this on a SIGHUP, so make it an MVar.
local_hostent :: MVar HostEntry
local_hostent = unsafePerformIO newEmptyMVar

