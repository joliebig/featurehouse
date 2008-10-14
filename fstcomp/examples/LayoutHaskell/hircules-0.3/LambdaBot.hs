{-# LINE 1 "LambdaBot.hsc" #-}
--  LambdaBot.hs: lambdabot integration
{-# LINE 2 "LambdaBot.hsc" #-}
--
--  Version: $Revision: 1.1 $ from $Date: 2008-10-14 18:01:23 $
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


{-# LINE 41 "LambdaBot.hsc" #-}

installModules :: IRC ()
botProcess :: Module m => m -> IRCMessage -> String -> String -> String -> IRC ()
botApology :: String -> String -> IRC ()


{-# LINE 71 "LambdaBot.hsc" #-}
installModules = return ()
botProcess _ _ _ _ _ = return ()
botApology _ _ = return ()

{-# LINE 75 "LambdaBot.hsc" #-}
