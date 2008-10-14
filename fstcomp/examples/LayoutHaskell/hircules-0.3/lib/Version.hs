--  Version.hs.in: version holder  -*-haskell-*-
--
--  Author : Jens-Ulrik Petersen
--  Created: 14 May 2003
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

module Version where

import System.Environment(getProgName)

versionString :: String
versionString = "0.3"

version :: IO ()
version =
    do
    progname <- getProgName
    putStrLn $ progname ++ " " ++ versionString

