-- |
-- Maintainer: Henning Guenther
--
-- A widget listing channels of a network, including their join and privacy status.
module Barracuda.GUI.ChannelList
	(ChannelList()
	,Channel(..)
	,channelListNew
	,channelListGetWidget
	,channelListSetChannels
	,onChannelSelect
	,onChannelJoin
	,onChannelLeave
	,onChannelCreate
	) where

import Control.Monad.Fix
import Data.IORef
import Data.List
import Network.AdHoc.Channel
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

-- | Discribes a ChannelList. In this datatype are the inner TreeView and the outer ScrolledWindow defined. 
-- Also there is the list of channels and some callbacks for user interaction.
data ChannelList = ChannelList
	{outer :: ScrolledWindow
	,inner :: TreeView
	,store :: New.ListStore Channel
	,fonCreate	 :: IORef (IO ())
	,fonJoin         :: IORef (Channel -> IO ())
	,fonLeave        :: IORef (Channel -> IO ())
	,fonSelect	 :: IORef (Channel -> IO ())
	,connectorSelect :: ConnectId TreeSelection
	}

-- | This datatype stores the name of a channel and the information if the user has joined and if
-- it is a private or non private channel.
data Channel = Chan
	{name :: String
	,channelName :: ChannelName
	,channelID :: ChannelID
	,private :: Bool
	,joined :: Bool
	}

-- | This function deletes all the channels in the given Channellist and replaces them with the given one. 
channelListSetChannels :: ChannelList -> [Channel] -> Maybe (ChannelName,ChannelID) -> IO ()
channelListSetChannels wid chans sel = do
	signalBlock (connectorSelect wid)
	let ind = (do
		(cname,cid) <- sel
		findIndex (\ch -> channelName ch == cname && channelID ch == cid) chans)
	New.listStoreClear (store wid)
	mapM_ (New.listStoreAppend (store wid)) chans
	case ind of
		Nothing -> return ()
		Just rind -> do
			selection <- New.treeViewGetSelection (inner wid)
			New.treeSelectionSelectPath selection [rind]
	signalUnblock (connectorSelect wid)

-- | Returns the ChannelList as a Widget
channelListGetWidget :: ChannelList -> Widget
channelListGetWidget = toWidget.outer

-- | Creates a new ChannelList with a list of Channels, the inner TreeView and the outer ScrolledWindow. 
-- Also there are some callbacks for the user interaction. 
channelListNew :: IO ChannelList
channelListNew = mfix $ \cl -> do
	ls <- New.listStoreNew ([]::[Channel])
	sw <- scrolledWindowNew Nothing Nothing
	sw `set` [scrolledWindowHscrollbarPolicy:=PolicyAutomatic
		 ,scrolledWindowVscrollbarPolicy:=PolicyAutomatic
		 ,scrolledWindowShadowType:=ShadowEtchedIn]
	tv <- New.treeViewNewWithModel ls
	col0 <- New.treeViewColumnNew
	col1 <- New.treeViewColumnNew
	col2 <- New.treeViewColumnNew
	New.treeViewSetHeadersVisible tv True
	New.treeViewColumnSetTitle col1 "Name"
	
	renderer0 <- New.cellRendererPixbufNew
	renderer1 <- New.cellRendererTextNew
	renderer2 <- New.cellRendererPixbufNew
	New.cellLayoutPackStart col0 renderer0 False
	New.cellLayoutPackStart col1 renderer1 True
	New.cellLayoutPackStart col2 renderer2 False
	New.cellLayoutSetAttributes col0 renderer0 ls
		$ \row -> [New.cellPixbufStockId:=if private row
			then "gtk-dialog-authentication"
			else ""]
	New.cellLayoutSetAttributes col1 renderer1 ls
		$ \row -> [New.cellText:=name row]
	New.cellLayoutSetAttributes col2 renderer2 ls
		$ \row -> [New.cellPixbufStockId:=if joined row
			then "gtk-connect"
			else "gtk-disconnect"
			]
	New.treeViewAppendColumn tv col2
	New.treeViewAppendColumn tv col0
	New.treeViewAppendColumn tv col1
	treeSel <- New.treeViewGetSelection tv
	tv `onButtonPress` (\ev -> do
		case eventButton ev of
			RightButton -> do
				men <- channelListCtxMenu cl (round $ eventX ev,round $ eventY ev)
				menuPopup men (Just (RightButton,eventTime ev))
				return False
			_ -> return False
		)
	cid <- treeSel `New.onSelectionChanged` (do
		f <- readIORef (fonSelect cl)
		sel <- New.treeSelectionGetSelectedRows treeSel
		case sel of
			[[row]] -> New.listStoreGetValue ls row >>= f
			_ -> return ()
		)
	tv `New.onRowActivated` (\[row] _ -> do
		f <- readIORef (fonJoin cl)
		New.listStoreGetValue ls row >>= f
		)
	sw `containerAdd` tv
	zeroFunc1 <- newIORef (return ())
	zeroFunc2 <- newIORef (const $ return ())
	zeroFunc3 <- newIORef (const $ return ())
	zeroFunc4 <- newIORef (const $ return ())
	return $ ChannelList sw tv ls zeroFunc1 zeroFunc2 zeroFunc3 zeroFunc4 cid

-- | Gives a callback if a user selects a channel in the list.
onChannelSelect :: ChannelList -> (Channel -> IO ()) -> IO ()
onChannelSelect wid f = writeIORef (fonSelect wid) f

-- | Gives a callback if a user clicks on Join in the context menu.
onChannelJoin :: ChannelList -> (Channel -> IO ()) -> IO ()
onChannelJoin wid f = writeIORef (fonJoin wid) f

-- | Gives a callback if a user clicks on Leave in the context menu.
onChannelLeave :: ChannelList -> (Channel -> IO ()) -> IO ()
onChannelLeave wid f = writeIORef (fonLeave wid) f

-- | Gives a callback if a user clicks on the <add Channel> button in the context menu.
onChannelCreate :: ChannelList -> IO () -> IO ()
onChannelCreate wid f = writeIORef (fonCreate wid) f
-- | This function is used to create the connect/disconnect and private/not private images.
channelListCtxMenu :: ChannelList -> Point -> IO Menu
channelListCtxMenu cl point = do
	sel <- New.treeViewGetPathAtPos (inner cl) point
	ch <- case sel of
		Nothing -> return Nothing
		Just ([row],_,_) -> New.listStoreGetValue (store cl) row >>= return.Just
	men <- menuNew
	case ch of
		Just chan -> if not (joined chan)
			then (do
				joinImg <- imageNewFromStock "gtk-connect" 1
				itJoin <- imageMenuItemNewWithLabel "Join"
				imageMenuItemSetImage itJoin joinImg
				itJoin `onActivateLeaf` (do
					f <- readIORef (fonJoin cl)
					f chan
					)
				menuShellAppend men itJoin
				)
			else (do
				leaveImg <- imageNewFromStock "gtk-disconnect" 1
				itLeave <- imageMenuItemNewWithLabel "Leave"
				imageMenuItemSetImage itLeave leaveImg
				itLeave `onActivateLeaf` (do
					f <- readIORef (fonLeave cl)
					f chan)
				menuShellAppend men itLeave
			)
		_ -> return ()
	createImg <- imageNewFromStock "gtk-add" 1
	itCreate <- imageMenuItemNewWithLabel "Create"
	imageMenuItemSetImage itCreate createImg
	itCreate `onActivateLeaf` (do
		f <- readIORef (fonCreate cl)
		f)
	menuShellAppend men itCreate
	widgetShowAll men
	return men
