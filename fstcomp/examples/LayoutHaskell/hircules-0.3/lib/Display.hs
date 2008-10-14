module Display (beep)
where

-- temporarily here until added to gtk2hs
beep :: IO ()
beep = gdk_beep

foreign import ccall unsafe "gdk_beep"
  gdk_beep :: IO ()
