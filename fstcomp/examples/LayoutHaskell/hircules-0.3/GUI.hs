--  GUI.hs: gtk UI for hircules IRC client
--
--  Author : Jens-Ulrik Petersen
--  Created: May 2003
--
--  Version: $Revision: 1.1 $ from $Date: 2008-10-14 18:01:22 $
--
--  Copyright (c) 2003 Jens-Ulrik Holger Petersen
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.

module GUI (addIRCchannel,
            alertchannel,
            allchannel,
            chanI,
            displayChannelTab,
            hideIRCchannel,
            newIRCchannel,
            rawchannel,
            setupGUI,
            timeStamp,
            writeTextLn,
            writeTextRaw,
            Interactive)
where

import Control.Concurrent
-- import Control.Exception
import Control.Monad (when, unless)
import Data.Char (toLower)
-- import Data.FiniteMap
-- import Data.Maybe (fromMaybe, fromJust, isJust)
-- import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import System.Time
import System.Locale

import Channel
import Debug
import EntryArea (setEditable)
import Gtk hiding (Event(..))  -- needed to hide `str' and `time'
import Hierarchy (toContainer)
import GtkKeymap
import MaybeDo
import Threads
import WordString
import TextBufferCreateTag (textBufferCreateTagBool)
import TextView (afterPasteClipboard)

-- tabchannel :: MVar (FiniteMap Int IRCChannel)
-- tabchannel = unsafePerformIO $ newMVar emptyFM

book :: Notebook
book = unsafePerformIO notebookNew

allchannel :: IRCChannel
allchannel = unsafePerformIO $ addIRCchannel "%all" "/" False

rawchannel :: IRCChannel
rawchannel = unsafePerformIO $ addIRCchannel "%raw" "/" False

alertchannel :: IRCChannel
alertchannel = unsafePerformIO $ addIRCchannel "%alert" "/" False

type MainWidget = Container

toMainWidget :: ContainerClass a => a -> MainWidget
toMainWidget = toContainer

mainwidget :: MVar MainWidget
mainwidget = unsafePerformIO $ newEmptyMVar

mainwindow :: Window
mainwindow = unsafePerformIO windowNew

setupGUI :: IO ()
setupGUI = do
  initGUI
  let window = mainwindow
  windowSetDefaultSize window 500 348
  windowSetTitle window "Hircules IRC client"
  onDelete window (const $ shutDown >> return True)
  notebookSetScrollable book True
  notebookSetPopup book True
  onSwitchPage book updateTabN
  let initwidget = book
  putMVar mainwidget $ toMainWidget initwidget
  containerAdd window initwidget
  widgetShowAll window
  keymap <- newKeymap
  mapM_ (keymapAdd keymap) globalKeyBindings
  onKeyPress window $ keyPressCB keymap
  timeoutAdd (yield >> return True) 10
  return ()

globalKeyBindings :: [Keybinding]
globalKeyBindings =
    [ KB [ModCtrl] "1" (switchToTab 0)
    , KB [ModCtrl] "2" (switchToTab 1)
    , KB [ModCtrl] "3" (switchToTab 2)
    , KB [ModCtrl] "4" (switchToTab 3)
    , KB [ModCtrl] "5" (switchToTab 4)
    , KB [ModCtrl] "6" (switchToTab 5)
    , KB [ModCtrl] "7" (switchToTab 6)
    , KB [ModCtrl] "8" (switchToTab 7)
    , KB [ModCtrl] "9" (switchToTab 8)
    , KB [ModCtrl] "0" (switchToTab 9)
    , KB [ModCtrl,ModShift] "W" hideCurrentChannel
    , KB [ModCtrl] "q" shutDown
    ]
  where
  switchToTab :: Int -> IO ()
  switchToTab n = do
      bookon <- widgetIsAncestor mainwindow book 
      unless bookon (switchTo book)
      notebookSetCurrentPage book n
  switchTo :: ContainerClass a => a -> IO ()
  switchTo widget = do
      displaying <- widgetIsAncestor mainwindow widget
      unless displaying $ do
        current <- takeMVar mainwidget
        containerRemove mainwindow current
        containerAdd mainwindow widget
        putMVar mainwidget $ toMainWidget widget

bufferKeyBindings :: IRCChannel -> [Keybinding]
bufferKeyBindings chan =
  [ KB [] "Return" (sendInput chan)]
  
addIRCchannel :: String -> String -> Bool -> IO IRCChannel
addIRCchannel title nick real = do
  chan <- newIRCchannel title nick real
  displayChannelTab True chan
  widgetGrabFocus $ chanview chan
  return chan

displayChannelTab :: Bool -> IRCChannel -> IO ()
displayChannelTab switch chan = do
  let mainbox = chanbox chan
  pageno <- notebookPageNum book mainbox
  page <- case pageno of
             (Just p) -> return p
             Nothing -> do
                  n <- notebookGetNPages book
                  let lbltxt = show (n + 1) ++ " " ++ (channame chan)
                  label <- labelNew $ Just lbltxt
                  notebookAppendPageMenu book mainbox label lbltxt
                  return n
  when switch $
      notebookSetCurrentPage book page

newIRCchannel :: String -> String -> Bool -> IO IRCChannel
newIRCchannel title nick real = do
--   mainbox <- vBoxNew False 5
  scrollwin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrollwin PolicyAutomatic PolicyAlways
--   boxPackStart mainbox scrollwin PackGrow 0
  buffer <- textBufferNew Nothing
  textBufferCreateTagBool buffer "not-editable" "editable" False
  textBufferCreateTagBool buffer "editable" "editable" True
  view <- textViewNewWithBuffer buffer
  textViewSetWrapMode view WrapWord
  textViewSetEditable view True
  containerAdd scrollwin view
  textBufferSetText buffer $ 
                    if real
                       then nick
                       else ""
--   nstart <- textBufferGetStartIter buffer
--   nickstart <- textBufferCreateMark buffer Nothing nstart True
  nend' <- textBufferGetEndIter buffer
  nickend' <- textBufferCreateMark buffer Nothing nend' True
--   textMarkSetVisible nickstart True
--   textMarkSetVisible nickend True
  textBufferInsert buffer nend' $
                    if real
                       then "@" ++ title ++ "> "
                       else title +-+ nick
  start' <- textBufferGetStartIter buffer
  textBufferInsert buffer start' "\n"
  start <- textBufferGetStartIter buffer
  end <- textBufferGetEndIter buffer
  textBufferApplyTagByName buffer "not-editable" start end
  endmark <- textBufferCreateMark buffer Nothing start False
  entry <- textBufferGetEndIter buffer
  entrymark <- textBufferCreateMark buffer Nothing entry True
  nend <- textBufferGetIterAtMark buffer nickend'
  nickend <- textBufferCreateMark buffer Nothing nend False
  let result = IRCChan {chanbuffer = buffer, channame = (map toLower title), chanreal = real, chanend = endmark, channick = nickend, chanbox = toMainWidget scrollwin, chanentry = entrymark, chanview = view, chanusers = [], chantopic = "", chancoding = Nothing}
  keymap <- newKeymap
  mapM_ (keymapAdd keymap) $ bufferKeyBindings result
  onKeyPress view $ keyPressCB keymap
  TextView.afterPasteClipboard view (setEditable result)
  widgetShowAll scrollwin
  return result

hideCurrentChannel :: IO ()
hideCurrentChannel = do
  notebookGetCurrentPage book >>= doRemoveNthPage

hideIRCchannel :: IRCChannel -> IO ()
hideIRCchannel chan = do
  total <- notebookGetNPages book
  if total <= 1
     then return ()
     else do
          let mainbox = chanbox chan
          p <- notebookPageNum book mainbox
          maybeDo p doRemoveNthPage

doRemoveNthPage :: Int -> IO ()
doRemoveNthPage page = do
  total <- notebookGetNPages book
  if total <= 1
     then return ()
     else do
          notebookRemovePage book page
          updateTabLabels

updateTabLabels :: IO ()
updateTabLabels = return ()

writeTextLn :: IRCChannel -> Bool -> String -> IO ()
writeTextLn chan alert str = do
  let buffer = chanbuffer chan
      endmark = chanend chan
  end <- textBufferGetIterAtMark buffer endmark
  newmark <- textBufferCreateMark buffer Nothing end True
  time <- timeStamp
  no_text <- textIterIsStart end
  textBufferInsert buffer end $ (if no_text then "" else "\n") ++ time ++ " " ++ str
  start <- textBufferGetIterAtMark buffer newmark
  end' <- textBufferGetIterAtMark buffer endmark
  textBufferApplyTagByName buffer "not-editable" start end'
--   let view = chanview chan
--   viewfocus <- widgetIsFocus view
--   unless viewfocus $ do
--       textViewScrollMarkOnscreen view endmark
--       textBufferPlaceCursor buffer end
--       displayChannelTab False chan
  textBufferGetInsert buffer >>= textViewScrollMarkOnscreen (chanview chan)
--   when (cursorAtEnd && not autoscroll)
--       (textBufferGetIterAtMark buffer endmark >>= textBufferPlaceCursor buffer)
  displayChannelTab False chan
  updateChannelTab chan alert

writeTextRaw :: String -> IO ()
writeTextRaw str = writeTextLn rawchannel True str

timeStamp :: IO String
timeStamp = do
  ct <- (getClockTime >>= toCalendarTime)
  return $ formatCalendarTime defaultTimeLocale (timeFmt defaultTimeLocale) ct

type Interactive = ([String], Channel)

chanI :: Chan Interactive
chanI = unsafePerformIO newChan

sendInput :: IRCChannel -> IO ()
sendInput chan = do
  let entry = chanentry chan
      buffer = chanbuffer chan
  start <- textBufferGetIterAtMark buffer entry
  end <- textBufferGetEndIter buffer
  text <- textBufferGetText buffer start end False
  debug "sendInput" text
  textBufferDelete buffer start end
  writeChan chanI (lines text, channame chan)

updateChannelTab :: IRCChannel -> Bool -> IO ()
updateChannelTab chan alert = do
  page <- notebookPageNum book mainbox
  current <- notebookGetCurrentPage book
  case page of
      (Just p) -> do
          let text = show (p + 1) ++ " " ++ (channame chan)
              markup = if alert && current /= p
                          then markSpan [FontForeground "red"]  text
                          else text
          label <- labelNew Nothing
          labelSetMarkup label markup
          notebookSetTabLabel book mainbox label
      Nothing -> return ()
  where
  mainbox = chanbox chan

highlightText :: String -> Markup
highlightText = markSpan [FontForeground "red"]

unhighlightText :: String -> String
unhighlightText str | whead str == open && wlast str == close =
                        take rawlength $ drop (length open) str
                    | otherwise = str
  where
  template = highlightText encl
  (open,close) = breakString encl template
  encl = "  "
  rawlength = length str - length open - length close

updateTabN :: Int -> IO ()
updateTabN n = do
     mw <- notebookGetNthPage book n
     case mw of
         Just w -> do
             mlbl <- notebookGetTabLabel book w
             case mlbl of
                   Just label -> do
                       txt <- labelGetText label
                       labelSetText label $ unhighlightText txt
                   Nothing -> return ()
         Nothing -> return ()
