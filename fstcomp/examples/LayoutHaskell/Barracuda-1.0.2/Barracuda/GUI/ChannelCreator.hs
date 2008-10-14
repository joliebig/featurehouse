-- |
-- Maintainer: Henning Guenther
--
-- A module containing a widget to create new channels.
module Barracuda.GUI.ChannelCreator
	(ChannelCreator()
	,channelCreatorNew
	,channelCreatorGetWidget
	,channelCreatorOnEnter
	) where

import Data.IORef
import Graphics.UI.Gtk

-- | A widget to create new channels.
data ChannelCreator = ChanCreator Table (IORef (String -> String -> Bool -> IO ()))

-- | Initiates a new 'ChannelCreator'.
channelCreatorNew :: IO ChannelCreator
channelCreatorNew = do
	cb <- newIORef (\_ _ _ -> return ())
	tab <- tableNew 5 2 False
	lblName <- labelNew (Just "<b>Channelname:</b>")
	lblName `set` [miscXalign:=0,labelUseMarkup:=True]
	entrName <- entryNew
	lblDescr <- labelNew (Just "<b>Description:</b>")
	lblDescr `set` [miscXalign:=0,labelUseMarkup:=True]
	entrDescr <- entryNew
	lblType <- labelNew (Just "<b>Channeltype:</b>")
	lblType `set` [miscXalign:=0,labelUseMarkup:=True]
	optPub <- radioButtonNewWithLabel "public"
	optPriv <- radioButtonNewWithLabelFromWidget optPub "private"
	butOk <- buttonNewFromStock stockAdd
	let rcb = do
		f <- readIORef cb
		name <- entrName `get` entryText
		descr <- entrDescr `get` entryText
		priv <- optPriv `get` toggleButtonActive
		f name descr priv
	butOk `onClicked` rcb
	tab `onKeyPress` (\ev -> case eventKeyName ev of
		"Return" -> rcb >> return True
		_ -> return False)
	entrName `onKeyPress` (\ev -> case eventKeyName ev of
		"Return" -> rcb >> return True
		_ -> return False)
	entrDescr `onKeyPress` (\ev -> case eventKeyName ev of
		"Return" -> rcb >> return True
		_ -> return False)
	butBox <- hButtonBoxNew
	butBox `set` [buttonBoxLayoutStyle:=ButtonboxEnd]
	boxPackStart butBox butOk PackNatural 0
	tableAttach tab lblName 0 1 0 1 [Fill] [Fill] 5 0
	tableAttach tab lblDescr 0 1 1 2 [Fill] [Fill] 5 0
	tableAttach tab lblType 0 1 2 4 [Fill] [Fill] 5 0
	tableAttach tab entrName 1 2 0 1 [Expand,Fill] [Fill] 0 0
	tableAttach tab entrDescr 1 2 1 2 [Expand,Fill] [Fill] 0 0
	tableAttach tab optPub 1 2 2 3 [Fill] [Fill] 0 0
	tableAttach tab optPriv 1 2 3 4 [Fill] [Fill] 0 0
	tableAttach tab butBox 0 2 4 5 [Fill] [Fill] 0 0
	return (ChanCreator tab cb)

-- | Sets the callback for a 'ChannelCreator'. It is called with channel name,
--   channel description and private flag, when the user confirms the data.
channelCreatorOnEnter :: ChannelCreator -> (String -> String -> Bool -> IO ()) -> IO ()
channelCreatorOnEnter (ChanCreator _ cb) f = writeIORef cb f

-- | Returns the 'Widget' associated with a 'ChannelCreator'.
channelCreatorGetWidget :: ChannelCreator -> Widget
channelCreatorGetWidget (ChanCreator tab _) = toWidget tab
