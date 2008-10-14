-- |
-- A module for supportive functions concerning the gui.
module Barracuda.GUI.Utils where

import Prelude hiding (catch)
import Control.Exception(evaluate,catch)
import Graphics.UI.Gtk.ModelView as New

-- | Returns the list of values stored in a 'New.ListStore'.
listStoreGetValues :: New.ListStore a -> IO [a]
listStoreGetValues lst = getValues 0 where
	getValues n = (do
		x <- New.listStoreGetValue lst n
		evaluate x
		xs <- getValues (n+1)
		return (x:xs)
		) `catch` (\_ -> return [])
