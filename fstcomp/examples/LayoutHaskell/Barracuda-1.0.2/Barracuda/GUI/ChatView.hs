-- |
-- Maintainer: Henning Guenther, Oliver Mielentz
--
-- The central gui widget displaying the messages of the channels.
module Barracuda.GUI.ChatView
	(ChatView
	,ChatContent
	,chatViewNew
	,chatContentNew
	,chatViewSetChatContent
	,chatViewGetWidget
	,chatViewScroll
	,chatContentInsert
	,chatContentWantAuth
	,chatContentError
	,onDownload
	,onAuth
	) where

import Data.Map as Map
import Graphics.UI.Gtk

import Control.Monad (when)
import Data.IORef
import Data.Time
import System.Locale
import Network.AdHoc.UserID
import Network.AdHoc.Message

-- | A 'ChatView' is a Widget that displays the content of a 'ChatContent'
data ChatView = ChatView ScrolledWindow TextView

-- | A 'ChatContent' represents a buffer with messages, e.g. for a specific
--   channel
data ChatContent = ChatContent
	{buffer           :: TextBuffer
	,tagtable         :: TextTagTable
	,tagBold          :: TextTag
	,tagWarn          :: TextTag
	,tagDelayed       :: TextTag
	,callbackDownload :: IORef ([Attachment] -> IO ())
	,callbackAuth     :: IORef (UserID -> IO ())
	,imgSave          :: Pixbuf
	,imgApply         :: Pixbuf
	}

-- | Retrieves the main Widget of a 'ChatView'. Can be used to insert the
--   'ChatView' into another Widget.
chatViewGetWidget :: ChatView -> Widget
chatViewGetWidget (ChatView wid _) = toWidget wid

-- | Sets the content of a 'ChatView'.
chatViewSetChatContent :: ChatView -> ChatContent -> IO ()
chatViewSetChatContent (ChatView _ cv) cc = textViewSetBuffer cv (buffer cc)

-- | Creates a 'ChatView' Widget without any content.
chatViewNew :: IO ChatView
chatViewNew = do
	win <- scrolledWindowNew Nothing Nothing
	win `set` [scrolledWindowVscrollbarPolicy := PolicyAlways
		  ,scrolledWindowHscrollbarPolicy := PolicyNever]
	cv <- textViewNew
	cv `set` [textViewEditable := False
		 ,textViewWrapMode := WrapWordChar
		 ,textViewCursorVisible := False
		 ]
	containerAdd win cv
	return (ChatView win cv)

chatContentTimestamp :: UTCTime -> ChatContent -> IO ()
chatContentTimestamp time cc = do
	timezone <- getCurrentTimeZone
	let time_str = formatTime defaultTimeLocale "%R" (utcToLocalTime timezone time)
	iter <- textBufferGetEndIter (buffer cc)
	textBufferInsert (buffer cc) iter time_str
	time_iter <- textIterCopy iter
	textIterBackwardChars time_iter (length time_str)
	textBufferApplyTag (buffer cc) (tagBold cc) iter time_iter

-- | Inserts a message with timestamp, text and attachments into a
--   'ChatContent'.
chatContentInsert :: UTCTime -> Maybe UserID -> String -> [Attachment] -> Bool -> ChatContent -> IO ()
chatContentInsert time sender txt attachments delayed cv = do
	chatContentTimestamp time cv
	iter <- textBufferGetEndIter (buffer cv)
	let prefix = case sender of
		Nothing -> " <anonymous>"
		Just rsender -> " <"++show rsender++">"
	textBufferInsert (buffer cv) iter prefix
	when delayed (do
		textBufferInsert (buffer cv) iter " [delayed]"
		delIter <- textIterCopy iter
		textIterBackwardChars delIter 10
		textBufferApplyTag (buffer cv) (tagDelayed cv) delIter iter)
	textBufferInsert (buffer cv) iter (" "++txt)
	case attachments of
		[] -> return ()
		_  -> do
			textBufferInsert (buffer cv) iter " "
			textBufferInsertPixbuf (buffer cv) iter (imgSave cv)
			picIter <- textIterCopy iter
			textIterBackwardChar picIter
			save_tag <- textTagNew Nothing
			save_tag `onTextTagEvent` (\ev _ -> case ev of
				Button _ SingleClick _ _ _ [] LeftButton _ _ -> do
					cb <- readIORef (callbackDownload cv)
					cb attachments
				_ -> return ()
				)
			textTagTableAdd (tagtable cv) save_tag
			textBufferApplyTag (buffer cv) save_tag iter picIter
			return ()
	textBufferInsert (buffer cv) iter "\n"

chatContentWantAuth :: UTCTime -> UserID -> ChatContent -> IO ()
chatContentWantAuth time from cc = do
	chatContentTimestamp time cc
	iter <- textBufferGetEndIter (buffer cc)
	textBufferInsert (buffer cc) iter (" "++show from++" wants to join. Allow? ")
	textBufferInsertPixbuf (buffer cc) iter (imgApply cc)
	picIter <- textIterCopy iter
	textIterBackwardChar picIter
	apply_tag <- textTagNew Nothing
	apply_tag `onTextTagEvent` (\ev _ -> case ev of
		Button _ SingleClick _ _ _ [] LeftButton _ _ -> do
			cb <- readIORef (callbackAuth cc)
			cb from
		_ -> return ())
	textTagTableAdd (tagtable cc) apply_tag
	textBufferApplyTag (buffer cc) apply_tag iter picIter
	textBufferInsert (buffer cc) iter ("\n")

-- | Creates an empty 'ChatContent'(no messages in it).
chatContentNew :: WidgetClass w => w -> IO ChatContent
chatContentNew wid = do
	Just pbApply <- widgetRenderIcon wid stockApply iconSizeMenu ""
	Just pbSave <- widgetRenderIcon wid stockSave iconSizeMenu ""
	tt <- textTagTableNew
	buf <- textBufferNew (Just tt)
	func <- newIORef (const $ return ())
	func2 <- newIORef (const $ return ())
	tb <- textTagNew Nothing
	tb `set` [textTagWeight := fromEnum WeightBold]
	tw <- textTagNew Nothing
	tw `set` [textTagForeground := "red"]
	td <- textTagNew Nothing
	td `set` [textTagForeground := "orange"]
	textTagTableAdd tt tb
	textTagTableAdd tt tw
	textTagTableAdd tt td
	return $ ChatContent
		{buffer = buf
		,tagtable = tt
		,tagBold = tb
		,tagWarn = tw
		,tagDelayed = td
		,callbackDownload = func
		,callbackAuth = func2
		,imgApply = pbApply
		,imgSave = pbSave
		}

chatContentError :: UTCTime -> String -> ChatContent -> IO ()
chatContentError time txt cc = do
	chatContentTimestamp time cc
	iter <- textBufferGetEndIter (buffer cc)
	textBufferInsert (buffer cc) iter (" "++txt)
	iter2 <- textIterCopy iter
	textIterBackwardChars iter2 (length txt)
	textBufferApplyTag (buffer cc) (tagWarn cc) iter iter2
	textBufferInsert (buffer cc) iter "\n"

-- | Sets a callback function to be called whenever the user wants to save
--   specific attachments.
onDownload :: ChatContent -> ([Attachment] -> IO ()) -> IO ()
onDownload cc f = modifyIORef (callbackDownload cc) (const f)

-- | Sets a callback function to be called whenever the user wants to
--   allow another user into the channel.
onAuth :: ChatContent -> (UserID -> IO ()) -> IO ()
onAuth cc f = modifyIORef (callbackAuth cc) (const f)

chatViewScroll :: ChatView -> IO ()
chatViewScroll (ChatView win cv) = do
	buf <- textViewGetBuffer cv
	iter <- textBufferGetEndIter buf
	textViewScrollToIter cv iter 0 Nothing
--	adj <- win `get` scrolledWindowVAdjustment
--	max <- adjustmentGetUpper adj
--	adjustmentSetValue adj max
	return ()
