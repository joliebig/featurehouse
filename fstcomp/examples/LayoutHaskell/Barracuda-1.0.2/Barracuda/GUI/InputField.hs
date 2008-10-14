-- |
-- Maintainer: Henning Guenther
--
-- The widget used to enter messages and attachments.
module Barracuda.GUI.InputField
	(InputField()
	,inputFieldNew
	,inputFieldGetWidget
	,inputFieldClear
	,onSend
	) where

import qualified Data.ByteString as BS
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk
import Data.IORef
import System.FilePath (splitFileName)
import Barracuda.GUI.Utils
import Network.AdHoc.Message (Attachment(..))

-- | A widget to enter messages with optional attachments.
data InputField = InputField
	{widget :: VBox
	,attachments :: New.ListStore File
	,sendButton :: Button
	,entry :: Entry
	,callback :: IORef (String -> [Attachment] -> IO ())
	}

-- | Defines a File as a tupel of the filename as a String and the location of a file as a FilePath.
data File = File
	{fileName :: String
	,fileLocation :: FilePath
	}
	deriving Show

fileToAttachment :: File -> IO Attachment
fileToAttachment file = do
	str <- BS.readFile (fileLocation file)
	return (Attachment (fileName file) "application/octet-stream" str)

-- | Creates a new InputField and returns it as a vBox
inputFieldNew :: IO InputField
inputFieldNew = do
	vbox <- vBoxNew False 0
	inpBox <- hBoxNew False 0
	inpText <- entryNew
	inpSend <- buttonNewWithLabel "Send"
	butImg <- imageNew
	imageSetFromStock butImg "gtk-execute" 1
	buttonSetImage inpSend butImg
	exp <- expanderNew "Attachments"
	ls <- New.listStoreNew []
	icons <- New.iconViewNewWithModel ls
	rendererDescr <- New.cellRendererTextNew
	rendererIcon <- New.cellRendererPixbufNew
	New.cellLayoutPackStart icons rendererIcon False
	New.cellLayoutPackStart icons rendererDescr False
	New.cellLayoutSetAttributes icons rendererDescr ls $ \row ->
		[New.cellText:=fileName row
		,New.cellWidthChars:=(-1)
		,New.cellXAlign:=0.5
		,New.cellEditable:=True
		]
	rendererDescr `New.onEdited` (\[tp] str -> do
		file <- New.listStoreGetValue ls tp
		New.listStoreSetValue ls tp (file {fileName=str})
		)
	New.cellLayoutSetAttributes icons rendererIcon ls
		$ \row -> [New.cellPixbufStockId:="gtk-file",New.cellPixbufStockSize:=2]
	New.iconViewSetSelectionMode icons SelectionMultiple
	New.iconViewSetRowSpacing icons 0
	icons `onKeyPress` (\ev -> do
		if eventKeyName ev == "Delete"
			then (do
				items <- New.iconViewGetSelectedItems icons
				mapM_ ((New.listStoreRemove ls).head) items
				return True
				)
			else return False
		)
	sw <- scrolledWindowNew Nothing Nothing
	sw `set` [scrolledWindowHscrollbarPolicy:=PolicyNever
		 ,scrolledWindowVscrollbarPolicy:=PolicyAutomatic
		 ,scrolledWindowShadowType:=ShadowEtchedIn]
	sw `containerAdd` icons
	exp `containerAdd` sw
	boxPackStart inpBox inpText PackGrow 0
	boxPackStart inpBox inpSend PackNatural 0
	boxPackStart vbox inpBox PackNatural 0
	boxPackStart vbox exp PackGrow 0
	cb <- newIORef (const $ const $ return ())
	let runCB = do
		text <- entryGetText inpText
		att <- listStoreGetValues ls >>= mapM fileToAttachment
		f <- readIORef cb
		f text att
	inpSend `onClicked` runCB
	inpText `onEntryActivate` runCB
	icons `onButtonPress` (\ev -> case ev of
		Button _ SingleClick time x y [] RightButton _ _ -> do
			men <- menuNew
			addImg <- imageNewFromStock "gtk-add" 1
			itAdd <- imageMenuItemNewWithLabel "Add file"
			imageMenuItemSetImage itAdd addImg
			menuShellAppend men itAdd
			itAdd `onActivateLeaf` (do
				dialog <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen [("gtk-cancel",ResponseCancel),("gtk-open",ResponseOk)]
				resp <- dialogRun dialog
				case resp of
					ResponseCancel -> return ()
					ResponseOk -> do
						fn <- fileChooserGetFilename dialog
						case fn of
							Nothing -> return ()
							Just rfn -> do
								let (dir,name) = splitFileName rfn
								New.listStoreAppend ls (File name rfn) 
				widgetDestroy dialog
				return ()
				)
			widgetShowAll men
			menuPopup men (Just (RightButton,time))
			return True
		_ -> return False
		)
	return (InputField vbox ls inpSend inpText cb)

-- | Returns the 'Widget' associated to an 'InputField'.
inputFieldGetWidget :: InputField -> Widget
inputFieldGetWidget ifi = toWidget (widget ifi)

-- | Sets a callback to be invoked whenever the user sends a message. Its
--   parameters are the actual text and a list of 'Attachment's.
onSend :: InputField -> (String -> [Attachment] -> IO ()) -> IO ()
onSend ifi f = writeIORef (callback ifi) f

-- | Deletes the content of an 'InputField'.
inputFieldClear :: InputField -> IO ()
inputFieldClear ifi = do
	(entry ifi) `set` [entryText := ""]
	New.listStoreClear (attachments ifi)
