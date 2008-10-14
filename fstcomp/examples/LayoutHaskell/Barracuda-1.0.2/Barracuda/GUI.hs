-- |
-- Maintainer: Oliver Mielentz, Henning Guenther
--
-- A module assembling the single gui modules to one user interface.
module Barracuda.GUI (
	guiNew
) where


import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Barracuda.GUI.CertificateLoader
import Barracuda.GUI.ChannelCreator
import Barracuda.GUI.ChannelList
import Barracuda.GUI.ChannelManager
import Barracuda.GUI.DownloadManager
import Barracuda.GUI.UserList
import Barracuda.GUI.InputField
import Barracuda.GUI.ChatView
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Time.Clock
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Barracuda.GUI.ServerInterface
import Network.AdHoc.Channel
import Network.AdHoc.UserID
import Network.AdHoc.Message
import Network.GnuTLS.X509
import System.Environment
import Data.ByteString (pack)
import Data.ByteString.Char8 (unpack)

-- | Spawns a new user interface.
guiNew :: GUI
guiNew send = do
	precv <- newIORef (const $ return ())
	win <- windowNew
	windowSetDefaultSize win 340 300
	cl <- loaderNew (\cert key -> case certificateGetUserID cert of
		Nothing -> error "Think of something here"
		Just name -> do
			f <- guiNew' name send
			writeIORef precv f
			send (SetUser name cert key)
			widgetDestroy win
		)
	win `containerAdd` (loaderGetWidget cl)
	widgetShowAll win
	return (\msg -> do
		f <- readIORef precv
		f msg)

guiNew' :: UserID -> GUI
guiNew' user send = do
	win <- windowNew
	let goldenRatio = (1 + sqrt (5 :: Double)) / (fromInteger 2)
	let height = 500
	windowSetDefaultSize win (round (goldenRatio * (fromInteger.toInteger) height)) height
	win `windowSetTitle` ("Barracuda - "++show user)
	manager <- channelManagerNew
		(\attach -> do
			dm <- downloadManagerNew attach
			win <- windowNew
			win `containerAdd` (downloadManagerGetWidget dm)
			widgetShowAll win)
		(\cname cid user -> send (Authorize user cname cid))
		user >>= newMVar

	-- generates the UserList Widget
	userList <- frameNew
	userl <- userListNew

	userlabel <- labelNew (Just "<b>Users in active Channel</b>")
	labelSetUseMarkup userlabel True
	frameSetLabelWidget userList userlabel
	userList `containerAdd` (userListGetWidget userl)

	-- generates the ChannelList Widget
	channel <- channelListNew
	channelList <- frameNew

	channellabel <- labelNew (Just "<b>Channels</b>")
	labelSetUseMarkup channellabel True
	frameSetLabelWidget channelList channellabel
	channelList `containerAdd` (channelListGetWidget channel)

	-- generates the InputField
	input <- inputFieldNew
	input `onSend` (\txt attach -> do
		cm <- readMVar manager
		case channelManagerChannel cm of
			Just (cname,cid,True) -> send (SendMsg cname cid txt attach)
			_ -> return ()
		inputFieldClear input)

		

	-- place the widgets
	hPan <- hPanedNew
	vbox <- vBoxNew False 0
	vPan_lists <- vPanedNew
	hPan `containerAdd` vPan_lists
	hPan `containerAdd` vbox
	chatviewframe <- frameNew
	chatviewlabel <- labelNew (Just "<b>Chat</b>")
	let changeMainWidget wid = do
		child <- binGetChild chatviewframe
		case child of
			Nothing -> return ()
			Just rchild -> containerRemove chatviewframe rchild
		containerAdd chatviewframe wid
		widgetShowAll chatviewframe
	let update cm = channelManagerCheckState cm changeMainWidget
	let updateUserList cm = userListSetUsers userl (channelManagerUsers cm)
	channel `onChannelSelect` (\chan -> modifyMVar_ manager (\cm -> do
		let ncm = channelManagerSelect (channelName chan) (channelID chan) cm
		nncm <- channelManagerCheckState ncm changeMainWidget
		updateUserList nncm
		chatViewScroll $ view nncm
		return nncm))
	channel `onChannelJoin` (\chan -> send (WantJoin (channelName chan) (channelID chan)))
	channel `onChannelLeave` (\chan -> send (WantLeave (channelName chan) (channelID chan)))
	channel `onChannelCreate` (do
		dialog <- windowNew
		windowSetTitle dialog "Create new channel"
		creat <- channelCreatorNew
		dialog `set`
			[windowTransientFor:=win
			,windowModal:=True
			,windowAllowGrow:=False]
		dialog `containerAdd` (channelCreatorGetWidget creat)
		creat `channelCreatorOnEnter` (\name descr priv -> do
			send (CreateChannel (mkChannelName name) descr (if priv then Just (Set.empty) else Nothing))
			widgetDestroy dialog)
		widgetShowAll dialog
		)

	labelSetUseMarkup chatviewlabel True
	frameSetLabelWidget chatviewframe chatviewlabel
	boxPackStart vbox chatviewframe PackGrow 0
	boxPackStart vbox (inputFieldGetWidget input) PackNatural 0

	set vPan_lists [ containerChild := frame 
		| frame <- [ channelList, userList ] ]
	vPan_lists `set` [panedPosition := 300]
	win `containerAdd` hPan
	withMVar manager update
	win `onDestroy` (do
		send CMClose
		modifyMVar_ manager (return.channelManagerNoState)
		)
	widgetShowAll win

	let receive msg = postGUIAsync $ case msg of
		AllChans mp -> do
			modifyMVar_ manager $ \cm -> update $ channelManagerUpdate mp cm
			cm <- readMVar manager
			channelListSetChannels channel (List.map
				(\((cname,cid),(desc,priv,users)) -> Chan
					(show cname ++ (case desc of
						"" -> ""
						_  -> " (" ++ desc ++ ")"))
					cname
					cid
					priv
					(Set.member (channelManagerUsername cm) users)) (Map.assocs mp)) (fmap (\(cname,cid,_)-> (cname,cid)) $ channelManagerChannel cm)
			updateUserList cm
		Receive cname cid sender msg attach time delayed -> modifyMVar_ manager $ channelManagerPost cname cid time sender msg attach delayed
		WantsAuth user cname cid -> do
			time <- getCurrentTime
			modifyMVar_ manager $ channelManagerWantsAuth cname cid time user
		ErrGeneral level title str -> do
			dia <- messageDialogNew (Just win) [] level ButtonsOk str
			windowSetTitle dia title
			dialogRun dia
			widgetDestroy dia
		ErrNotDelivered user cname cid msg _ time -> modifyMVar_ manager $
			channelManagerError cname cid time ((constructAnd $ fmap show user) ++ " didn't get your message.")
		_ -> print msg 
	return receive

constructAnd :: [String] -> String
constructAnd = constructAnd' ""
	where
	constructAnd' :: String -> [String] -> String
	constructAnd' str [] = str
	constructAnd' str (x:xs)
		| List.null str  = constructAnd' x xs
		| List.null xs   = str++" and "++x
		| otherwise      = constructAnd' (str++", "++x) xs
