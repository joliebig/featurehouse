module MaybeDo (maybeDo) where

maybeDo :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeDo mb act = maybe (return ()) act mb
