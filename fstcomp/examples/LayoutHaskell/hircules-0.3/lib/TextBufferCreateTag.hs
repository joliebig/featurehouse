module TextBufferCreateTag
where

import Monad	(liftM)
import FFI
import GObject	(makeNewGObject)
import Hierarchy
import TextTag

textBufferCreateTagBool :: TextBuffer -> TagName -> String -> Bool -> IO TextTag
textBufferCreateTagBool tb tname prop val = 
  withUTFString tname $ \ cStr ->
    withUTFString prop $ \ pStr ->
    makeNewGObject mkTextTag $ liftM castPtr $ (\(TextBuffer arg1) arg2 arg3 arg4 arg5 -> withForeignPtr arg1 $ \ argPtr1 -> text_buffer_create_tag argPtr1 arg2 arg3 arg4 arg5) tb cStr pStr (fromBool val) nullPtr

foreign import ccall unsafe "gtk_text_buffer_create_tag"
  text_buffer_create_tag :: Ptr TextBuffer -> Ptr CChar -> Ptr CChar ->
                            CInt -> Ptr () -> IO (Ptr TextTag)
