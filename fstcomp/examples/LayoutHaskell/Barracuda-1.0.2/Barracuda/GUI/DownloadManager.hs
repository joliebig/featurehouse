{- | The download manager displays attachments to the user and lets them save
     them.
 -}
module Barracuda.GUI.DownloadManager
	(DownloadManager()
	,downloadManagerNew
	,downloadManagerGetWidget
	) where

import Network.AdHoc.Message

import Data.ByteString as BS
import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import System.FilePath
import Prelude hiding (catch)
import Control.Exception(catch)

-- | A widget to save attachments of messages.
data DownloadManager = DLManager
	{store :: New.ListStore Attachment
	,outer :: VBox
	}

-- | Creates a new 'DownloadManager' with a list of attachments to download.
downloadManagerNew :: [Attachment] -> IO DownloadManager
downloadManagerNew attach = do
	ls <- New.listStoreNew attach
	box <- vBoxNew False 0
	sw <- scrolledWindowNew Nothing Nothing
	sw `set` [scrolledWindowHscrollbarPolicy:=PolicyAutomatic
		 ,scrolledWindowVscrollbarPolicy:=PolicyAutomatic
		 ,scrolledWindowShadowType:=ShadowEtchedIn]
	tv <- New.treeViewNewWithModel ls
	selection <- New.treeViewGetSelection tv
	selection `set` [New.treeSelectionMode:=SelectionMultiple]
	New.treeSelectionSelectAll selection
	colName <- New.treeViewColumnNew
	New.treeViewSetHeadersVisible tv False
	rendererName <- New.cellRendererTextNew
	New.cellLayoutPackStart colName rendererName True
	New.cellLayoutSetAttributes colName rendererName ls
		$ \row -> [New.cellText:=attachmentFilename row]
	New.treeViewAppendColumn tv colName
	sw `containerAdd` tv
	boxPackStart box sw PackGrow 0
	but_save <- buttonNewFromStock stockSave
	boxPackStart box but_save PackNatural 0
	but_save `onClicked` (do
		dialog <- fileChooserDialogNew Nothing Nothing
			FileChooserActionSelectFolder
			[(stockSave,ResponseOk)]
		resp <- dialogRun dialog
		case resp of
			ResponseOk -> do
				Just path <- fileChooserGetCurrentFolder dialog
				rows <- New.treeSelectionGetSelectedRows selection
				mapM_ (\[num] -> do
					attachment <- New.listStoreGetValue ls num
					
					(BS.writeFile
						(path </> (attachmentFilename attachment))
						(attachmentContent attachment))
						`catch`
						(\ex -> do
							dia <- messageDialogNew Nothing []
								MessageError ButtonsOk ("Failed to save attachment: "++show ex)
							windowSetTitle dia "Error"
							print "ERROR"
							dialogRun dia
							widgetDestroy dia)
					) rows
				widgetDestroy dialog
			_ -> widgetDestroy dialog
		)
	return $ DLManager 
		{store=ls
		,outer=box
		}

-- | Gets the 'Widget' of a 'DownloadManager'.
downloadManagerGetWidget :: DownloadManager -> Widget
downloadManagerGetWidget dl = toWidget (outer dl)
