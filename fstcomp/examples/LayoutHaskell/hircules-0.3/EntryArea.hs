module EntryArea
where

import Channel
import Gtk

setEditable :: IRCChannel -> IO ()
setEditable chan = do
  let mark = chanentry chan
      buffer = chanbuffer chan
  start <- textBufferGetIterAtMark buffer mark
  end <- textBufferGetEndIter buffer
  textBufferApplyTagByName buffer "editable" start end

setNickText :: IRCChannel -> String -> IO ()
setNickText chan nick = do
  let endm = channick chan
  start <- getNickStart
  end <- textBufferGetIterAtMark buffer endm
  textBufferDelete buffer start end
  begin <- getNickStart
  textBufferInsert buffer begin nick
  where
  endc = chanend chan
  buffer = chanbuffer chan
  getNickStart :: IO TextIter
  getNickStart = do
    start <- textBufferGetIterAtMark buffer endc
    textIterForwardChar start
    return start
