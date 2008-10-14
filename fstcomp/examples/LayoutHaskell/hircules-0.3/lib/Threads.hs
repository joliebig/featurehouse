--  Threads.hs: threads accounting
--
--  Author : Jens-Ulrik Petersen
--  Created: May 2003
--
--  Version: $Revision: 1.1 $ from $Date: 2008-10-14 18:01:21 $
--
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.

module Threads (killThreads, newThread, shutDown) where

import Control.Concurrent
import Control.Concurrent.MVar (MVar, newMVar)
import System.IO.Unsafe (unsafePerformIO)

import Gtk (mainQuit)

threads :: MVar [ThreadId]
threads = unsafePerformIO $ newMVar []

newThread :: IO () -> IO ThreadId
newThread io = do
    t <- forkIO io
    modifyMVar_ threads (\ ts -> return (t:ts))
    return t

killThreads :: IO ()
killThreads = do
    ts <- takeMVar threads
    mapM_ killThread ts

shutDown :: IO ()
shutDown = do
  mainQuit
  killThreads

