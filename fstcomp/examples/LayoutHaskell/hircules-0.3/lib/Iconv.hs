{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Text.Encoding.Iconv
-- Copyright   : (c) Martin Norbäck 2003
-- License     : Public Domain
--
-- Maintainer  : haskell-i18n@haskell.org
-- Stability   : experimental
-- Portability : non-portable (FFI extensions, UNIX98)
--
-- This module provides an interface to the iconv(3) functionality
--
-----------------------------------------------------------------------------

module Iconv
  ( iconv
  , IconvResult(..)
  ) where

import Foreign
import Foreign.C
import System.IO.Unsafe(unsafePerformIO)

data IconvResult
  = Success [Word8]       -- ^The conversion was successful
  | UnsupportedConversion -- ^The choosen conversion is not supported
  | InvalidSequence Int   -- ^There was an invalid input sequence, at this index
  | IncompleteSequence Int-- ^There was an incomplete input sequence at
                          --  the end of the input, at this index
  | Unknown               -- ^ There was an unknown error
    deriving Show
 
-- |
-- Convert a sequence of bytes from one encoding to another
iconv :: String  -- ^ The encoding to convert from
      -> String  -- ^ The encoding to convert to
      -> [Word8] -- ^ The byte sequence to convert
      -> IconvResult
iconv fromcode tocode bytes = unsafePerformIO $ do
  -- Create a conversion descriptor
  fromcode_c <- newCString fromcode
  tocode_c   <- newCString tocode
  descriptor <- _iconv_open tocode_c fromcode_c
  free fromcode_c
  free tocode_c
  -- check for error
  if iconv_t_error descriptor
    then return UnsupportedConversion
    else do
      -- just use an output size of 4 times the input size
      -- hopefully this is enough, otherwise ERROR ERROR
      Just res <- iconv_loop descriptor bytes (4*length bytes)
      _iconv_close descriptor 
      return res

iconv_loop :: Iconv_t -> [Word8] -> Int -> IO (Maybe IconvResult)
iconv_loop descriptor input outbufsize = do
  let inbufsize = length input
  -- setup all the pointers that iconv needs
  outbufPtr <- mallocBytes outbufsize
  outbufPtrPtr <- malloc
  outbytesleftPtr <- malloc
  poke outbufPtrPtr outbufPtr
  poke outbytesleftPtr outbufsize

  inbufPtr    <- newArray input 
  inbufPtrPtr <- malloc
  inbytesleftPtr <- malloc
  poke inbufPtrPtr inbufPtr
  poke inbytesleftPtr inbufsize

  result <- _iconv descriptor 
                   inbufPtrPtr inbytesleftPtr
                   outbufPtrPtr outbytesleftPtr
  
  inbytesleft  <- peek inbytesleftPtr
  outbytesleft <- peek outbytesleftPtr

  retval <- if result == (-1)
              then do
                let index = inbufsize - inbytesleft
                err <- getErrno
                case err of
                  _ | err == eILSEQ -> return (Just (InvalidSequence index))
                  _ | err == eINVAL -> return (Just (IncompleteSequence index))
                  _ | err == e2BIG  -> return Nothing
                  _      -> return (Just Unknown)
              -- now, the conversion has succeded, marshal output buffer
              else do
                result' <- peekArray (outbufsize - outbytesleft) outbufPtr
                return (Just (Success result'))

  free outbufPtr
  free outbufPtrPtr
  free outbytesleftPtr
  free inbufPtr
  free inbufPtrPtr
  free inbytesleftPtr

  return retval

-- no need to know anything about the contents of this type
-- except that it is (-1) on error

newtype Iconv_t = Iconv_t Int

iconv_t_error :: Iconv_t -> Bool
iconv_t_error (Iconv_t i) = i == (-1)

foreign import ccall "iconv.h iconv_open"
  _iconv_open :: Ptr CChar -> Ptr CChar -> IO Iconv_t
foreign import ccall "iconv.h iconv_close"
  _iconv_close :: Iconv_t -> IO Int
foreign import ccall "iconv.h iconv"
  _iconv :: Iconv_t 
         -> Ptr (Ptr Word8) -> Ptr Int 
         -> Ptr (Ptr Word8) -> Ptr Int 
         -> IO Int
