-- |
-- Maintainer: Henning Guenther, Oliver Mielentz
--
-- A widget to display the list of users in a selected channel.
module Barracuda.GUI.UserList 
	(UserLs()
	--,UserID(..)
	,userListNew
	,userListSetUsers
	,userListGetWidget
	)
	where

import Control.Monad.Fix
import Data.IORef
import Network.AdHoc.UserID
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

data UserLs = UserLs 
	{store :: New.ListStore UserID
	,outer :: ScrolledWindow
	,inner :: TreeView
	}

--data UserID = UserID { name :: String}
-- | Creates a new UserList which contains the list of users and an inner TreeView and an outer ScrolledWindow.
userListNew :: IO UserLs
userListNew = do
	userList <- New.listStoreNew ([] :: [UserID])
	sw <- scrolledWindowNew Nothing Nothing
	sw `set` [scrolledWindowHscrollbarPolicy:=PolicyAutomatic
		 ,scrolledWindowVscrollbarPolicy:=PolicyAutomatic
		 ,scrolledWindowShadowType:=ShadowEtchedIn]
	tv <- New.treeViewNewWithModel userList
	col0 <- New.treeViewColumnNew
	col1 <- New.treeViewColumnNew
	New.treeViewSetHeadersVisible tv False
	
	renderer0 <- New.cellRendererPixbufNew
	renderer1 <- New.cellRendererTextNew

	New.cellLayoutPackStart col0 renderer0 False
	New.cellLayoutPackStart col1 renderer1 True
	New.cellLayoutSetAttributes col0 renderer0 userList
		$ \row -> [New.cellPixbufIconName:="stock_person"]
	New.cellLayoutSetAttributes col1 renderer1 userList
		$ \row -> [New.cellText:= show row]

	New.treeViewAppendColumn tv col0
	New.treeViewAppendColumn tv col1
	sw `containerAdd` tv
	return $ UserLs userList sw tv

-- | This functions deletes the old UserList and replaces it with a new one given to the function.
userListSetUsers :: UserLs -> [UserID] -> IO ()
userListSetUsers ul users = do
	New.listStoreClear(store ul)
	mapM_ (New.listStoreAppend (store ul)) users
-- | Returns the UserList as a Widget.
userListGetWidget :: UserLs -> Widget
userListGetWidget = toWidget.outer
