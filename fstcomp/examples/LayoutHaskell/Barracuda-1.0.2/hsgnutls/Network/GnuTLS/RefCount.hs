module Network.GnuTLS.RefCount 
    (RefCount, newRefCount, allocRef, freeRef, addRefFinalizer)
    where

import Data.IORef

-- | A simple reference count with a list of finalizers.
newtype RefCount = RC (IORef (Int, [IO ()]))

-- | Create a new reference count with one reference and 
-- the suplied action as the initial finalizer.
newRefCount :: IO () -> IO RefCount
newRefCount act = return . RC =<< newIORef (1, [act])

-- | Allocate an additional reference to the RefCount.
allocRef :: RefCount -> IO ()
allocRef (RC ref) = atomicModifyIORef ref $ \(iv,cs) -> ((iv+1,cs),())

-- | Free a reference to the RefCount. If the number 
-- of references goes down to zero all the associated
-- finalizers are fired. The RefCount is left in 
-- unusable state when all the references have been 
-- deleted.
freeRef :: RefCount -> IO ()
freeRef (RC ref)  = atomicModifyIORef ref handler >>= sequence_
    where handler (1,cs) = (error "Double free of RefCount",cs)
          handler (k,cs) = ((k-1,cs), [])

-- | Associate an additional finalizer with the RefCount.
addRefFinalizer :: RefCount -> IO () -> IO ()
addRefFinalizer (RC ref) c = atomicModifyIORef ref $ \(iv,cs) -> ((iv,c:cs),())
