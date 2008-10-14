--  LambdaBot.hs: lambdabot integration
--
--  Version: $Revision: 1.2 $ from $Date: 2003/07/17 07:43:40 $
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

module LambdaBot where

import IRC

#ifdef LAMBDABOT
import Control.Monad.Error (catchError)
import Control.Monad.Trans (liftIO)
import System.IO (hPutStr, stderr)

-- import HelloModule
-- import FactModule
-- import TopicModule
-- import FortuneModule
-- import KarmaModule
-- import ChessModule
import DictModule
import EvalModule
import MoreModule
-- import QuoteModule
-- import SearchMLModule
import StateModule
import SystemModule
import TypeModule
#endif

installModules :: IRC ()
botProcess :: Module m => m -> IRCMessage -> String -> String -> String -> IRC ()
botApology :: String -> String -> IRC ()

#ifdef LAMBDABOT
installModules = do
    -- ircInstallModule helloModule
    -- ircInstallModule factModule
    -- ircInstallModule topicModule
    -- ircInstallModule fortuneModule
    -- ircInstallModule karmaModule
    -- ircInstallModule chessModule
-- causes flooding without doPRIVMSG hack
--     ircInstallModule dictModule
    ircInstallModule evalModule
-- needs doPRIVMSG hack
--     ircInstallModule moreModule
    -- ircInstallModule quoteModule
    -- ircInstallModule searchMLModule
    ircInstallModule stateModule
    ircInstallModule systemModule
    ircInstallModule typeModule

botProcess m msg who cmd tale =
    catchError (process m msg who cmd tale)
                 (\e -> liftIO $ hPutStr stderr $ show e)

botApology who txt = ircPrivmsg who txt
#else
installModules = return ()
botProcess _ _ _ _ _ = return ()
botApology _ _ = return ()
#endif
