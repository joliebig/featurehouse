-- -*-haskell-*-
--  GIMP Toolkit (GTK) Attributes interface
--
--  Author : Duncan Coutts
--
--  Created: 21 January 2005
--
--  Copyright (C) 2005 Duncan Coutts
--
--  Partially derived from the hs-fltk and wxHaskell projects which
--  are both under LGPL compatible licenses.
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : experimental
-- Portability : uses MPTC with functional dependencies
--
-- |
--
module Network.GnuTLS.Attributes (
  -- * Attribute classes
  ReadableAttribute(getter),
  WritableAttribute(setter),

  -- * Interface for getting, setting and updating attributes
  AttrOp(..),
  get,
  set,
  newAttr, readAttr, writeAttr,
  Attr,
  ReadAttr,
  WriteAttr,
  ReadWriteAttr
  ) where

infixr 0 :=,:~,:=>,:~>,::=,::~


class ReadableAttribute a m o d | a -> m, a -> o, a -> d where
  getter :: a -> o -> m d

class WritableAttribute a m o d | a -> m, a -> o, a -> d where
  setter :: a -> o -> d -> m ()


-- | A set or update operation on an attribute.
data Monad m => AttrOp m o
  = forall attr r w.
      WritableAttribute attr m o w =>
        attr := w                       -- ^ Assign a value to an attribute.

  | forall attr r w.
      (ReadableAttribute attr m o r
      ,WritableAttribute attr m o w) =>
        attr :~ (r -> w)                -- ^ Apply an update function to an
                                        --   attribute.
  | forall attr r w.
      WritableAttribute attr m o w =>
        attr :=> m w                    -- ^ Assign the result of an action to
                                        --   an attribute.
  | forall attr r w.
      (ReadableAttribute attr m o r
      ,WritableAttribute attr m o w) =>
        attr :~> (r -> m w)             -- ^ Apply a update action to an
                                        --   attribute.
  | forall attr r w.
      WritableAttribute attr m o w =>
        attr ::= (o -> w)               -- ^ Assign a value to an attribute
                                        --   with the object as an argument.

  | forall attr r w.
      (ReadableAttribute attr m o r
      ,WritableAttribute attr m o w) =>
        attr ::~ (o -> r -> w)          -- ^ Apply an update function to an
                                        --   attribute with the object as an
                                        --   argument.


-- | Do a number of attribute set or update operation for some object.
set :: forall m o. Monad m => o -> [AttrOp m o] -> m ()
set obj = mapM_ app
 where
   app :: AttrOp m o -> m ()
   app (attr :=  x) = setter attr obj x
   app (attr :~  f) = getter attr obj >>= \v -> setter attr obj (f v)
   app (attr :=> x) =                x >>= setter attr obj
   app (attr :~> f) = getter attr obj >>= f >>= setter attr obj
   app (attr ::= f) = setter attr obj (f obj)
   app (attr ::~ f) = getter attr obj >>= \v -> setter attr obj (f obj v)


-- | Get an Attr of an object.
get :: (Monad m, ReadableAttribute attr m o a) => o -> attr -> m a
get o attr = getter attr o


set_ :: Monad m => [AttrOp m ()] -> m ()
set_ = set ()

get_ :: (Monad m, ReadableAttribute attr m () a) => attr -> m a
get_ = get ()

{-
[3. text/x-haskell; IOAttributes.hs]

{-# OPTIONS -fglasgow-exts #-}

-- -*-haskell-*-
--  GIMP Toolkit (GTK) Attributes interface
--
--  Author : Duncan Coutts
--
--  Created: 21 January 2005
--
--  Copyright (C) 2005 Duncan Coutts
--
--  Partially derived from the hs-fltk and wxHaskell projects which
--  are both under LGPL compatible licenses.
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : experimental
-- Portability : portable
--
-- |
--
module System.Glib.IOAttributes (
  -- * Attribute types
  Attr,
  ReadAttr,
  WriteAttr,
  ReadWriteAttr,

  -- * Interface for getting, setting and updating attributes
  module System.Glib.Attributes,
  
  -- * Internal attribute constructors
  newAttr,
  readAttr,
  writeAttr,
  ) where

import System.Glib.Attributes
-}

-- | An ordinary attribute. Most attributes have the same get and set types.
type Attr o a = ReadWriteAttr o a a

-- | A read-only attribute.
type ReadAttr o a = ReadWriteAttr o a ()

-- | A write-only attribute.
type WriteAttr o b = ReadWriteAttr o () b

-- | A generalised attribute with independent get and set types.
data ReadWriteAttr o a b = Attr !(o -> IO a) !(o -> b -> IO ())


instance ReadableAttribute (ReadWriteAttr o a b) IO o a where
  getter (Attr getter' _) = getter'


instance WritableAttribute (ReadWriteAttr o a b) IO o b where
  setter (Attr _ setter') = setter'

-- | Create a new attribute with a getter and setter function.
newAttr :: (o -> IO a) -> (o -> b -> IO ()) -> ReadWriteAttr o a b
newAttr getter setter = Attr getter setter

-- | Create a new read-only attribute.
readAttr :: (o -> IO a) -> ReadAttr o a
readAttr getter = Attr getter (\_ _ -> return ())

-- | Create a new write-only attribute.
writeAttr :: (o -> b -> IO ()) -> WriteAttr o b
writeAttr setter = Attr (\_ -> return ()) setter

