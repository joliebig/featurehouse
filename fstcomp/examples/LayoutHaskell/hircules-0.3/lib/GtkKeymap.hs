--  A small keymap library for gtk
--
--  Author : Jens-Ulrik Petersen
--  Created: 15 July 2002
--
--  Version:  $Revision: 1.1 $ from $Date: 2008-10-14 18:01:21 $
--
--  Copyright (c) 2002 Jens-Ulrik Holger Petersen
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- Description
--

module GtkKeymap (keymapAdd, keyPressCB, newKeymap,
    Keymap, Keybinding(..), ModSym(..))
where
import FiniteMap
import MVar (newMVar, modifyMVar_, readMVar, MVar)

import Events

-- import Debug
import GdkKeys

type Keymap = MVar KeymapHash

type KeymapHash = FiniteMap (String, Modifier) (IO ())

data Keybinding = KB [ModSym] String (IO ())

-- need to map meta, alt, hyper, super, et al
data ModSym = ModShift | ModLock | ModCtrl | Mod1 | Mod2 | Mod3 | Mod4 | Mod5
  deriving Enum

newKeymap :: IO Keymap
newKeymap = newMVar emptyFM

keymapAdd :: Keymap -> Keybinding -> IO ()
keymapAdd keymap (KB modi name act) =
    modifyMVar_ keymap $ \keyfm -> do
--       debug $ symsToInt modi
      return $ addToFM keyfm (name, symsToInt modi) act
  where
      symsToInt :: [ModSym] -> Modifier
      symsToInt ss = foldl (+) 0 $ map (\s -> 2^(fromEnum s)) ss

keyPressCB :: Keymap -> Event -> IO Bool
keyPressCB keymap ev =
    do
    keyfm <- readMVar keymap
    let modi = modif ev
	keyname = keyvalName $ keyval ev
--     debug modi
--     debug "keyCB"keyname
    case keyname of
	 Just key ->
	   case lookupFM keyfm (key, modi) of
		Just act -> do act
		               return True
		Nothing -> return False
	 Nothing -> return False
