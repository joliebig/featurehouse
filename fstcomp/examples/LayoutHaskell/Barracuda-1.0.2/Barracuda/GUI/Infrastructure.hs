{- | A widget to manipulate the infrastructure mode of the node.
 -}
module Barracuda.GUI.Infrastructure
	(InfrastructureWidget()
	,infrastructureWidgetNew
	,infrastructureWidgetGet
	,onUpdate)
	where

import Barracuda.GUI.Utils
import Barracuda.Utils
import Network.Socket

import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New

data Infrastructure = Infrastructure
	{list :: New.ListStore SockAddr
	,view :: New.TreeView
	}

-- | A widget to edit the infrastructural data.
data InfrastructureWidget = InfrastructureWidget
	{ips :: Infrastructure
	,outer :: VBox
	,fonUpdate :: IORef (Maybe [SockAddr] -> IO ())
	}

-- | Creates a new 'InfrastructureWidget'.
infrastructureWidgetNew :: IO InfrastructureWidget
infrastructureWidgetNew = do
	inf <- infrastructureNew
	box <- vBoxNew False 0
	butActive <- checkButtonNewWithLabel "infrastructure mode active"
	boxPackStart box butActive PackNatural 0
	sw <- scrolledWindowNew Nothing Nothing
	sw `set` [scrolledWindowHscrollbarPolicy := PolicyNever
		 ,scrolledWindowVscrollbarPolicy := PolicyAutomatic
		 ,scrolledWindowShadowType := ShadowEtchedIn]
	sw `containerAdd` (view inf)
	boxPackStart box sw PackGrow 0
	entrBox <- hBoxNew False 0
	entrIp <- entryNew
	entrIp `set` [entryWidthChars := 15]
	boxPackStart entrBox entrIp PackGrow 0
	lblDot <- labelNew (Just ":")
	boxPackStart entrBox lblDot PackNatural 0
	entrPort <- entryNew
	entrPort `set` [entryWidthChars := 6]
	boxPackStart entrBox entrPort PackGrow 0
	addBut <- buttonNewFromStock stockAdd
	addBut `onClicked` (do
		ip <- entrIp `get` entryText
		port <- entrPort `get` entryText
		case (readSockAddr 8888 (ip++":"++port)) of
			Nothing -> return ()
			Just addr -> do
				infrastructureAdd inf addr
				entrPort `set` [entryText:=""]
				entrIp   `set` [entryText:=""]
		)
	boxPackStart entrBox addBut PackGrow 0
	boxPackStart box entrBox PackNatural 0
	f <- newIORef (const (return ()))
	saveBut <- buttonNewFromStock stockSave
	saveBut `onClicked` (do
		act <- butActive `get` toggleButtonActive
		cb <- readIORef f
		if act then (do
			lst <- listStoreGetValues (list inf)
			cb (Just lst)) else (cb Nothing)
		)
	boxPackStart box saveBut PackNatural 0
	let togg yes = do
		widgetSetSensitivity (view inf) yes
		widgetSetSensitivity addBut yes
		widgetSetSensitivity entrIp yes
		widgetSetSensitivity entrPort yes
	butActive `onToggled` (do
		act <- butActive `get` toggleButtonActive
		togg act)
	togg False
	return $ InfrastructureWidget inf box f

infrastructureNew :: IO Infrastructure
infrastructureNew = do
	ls <- New.listStoreNew []
	tv <- New.treeViewNewWithModel ls
	colIP   <- New.treeViewColumnNew
	colPort <- New.treeViewColumnNew
	New.treeViewColumnSetTitle colIP "IP"
	New.treeViewColumnSetTitle colPort "Port"
	rendererIP   <- New.cellRendererTextNew
	rendererIP `set` [New.cellWidthChars:=15]
	rendererPort <- New.cellRendererTextNew
	rendererPort `set` [New.cellWidthChars:=6]
	New.cellLayoutPackStart colIP   rendererIP   True
	New.cellLayoutPackStart colPort rendererPort True
	New.cellLayoutSetAttributes colIP rendererIP ls
		$ \(SockAddrInet _ host) -> [New.cellText:=showHost host]
	New.cellLayoutSetAttributes colPort rendererPort ls
		$ \(SockAddrInet port _) -> [New.cellText:=show port]
	New.treeViewAppendColumn tv colIP
	New.treeViewAppendColumn tv colPort
	tv `onKeyPress` (\ev -> if eventKeyName ev == "Delete"
		then (do
			sel <- New.treeViewGetSelection tv
			rows <- New.treeSelectionGetSelectedRows sel
			mapM_ (\[row] -> New.listStoreRemove ls row) rows
			return True
			)
		else return False)
	return $ Infrastructure
		{list = ls
		,view = tv
		}

infrastructureAdd :: Infrastructure -> SockAddr -> IO ()
infrastructureAdd inf addr = New.listStoreAppend (list inf) addr

{- | Sets the function to be called whenever the user sets new infrastructure
     informations.
 -}
onUpdate :: InfrastructureWidget -> (Maybe [SockAddr] -> IO ()) -> IO ()
onUpdate inf f = writeIORef (fonUpdate inf) f

-- | Returns the associated 'Widget'.
infrastructureWidgetGet :: InfrastructureWidget -> Widget
infrastructureWidgetGet = toWidget.outer
