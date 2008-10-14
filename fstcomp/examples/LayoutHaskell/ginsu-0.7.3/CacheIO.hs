
-- arch-tag: e415587a-fa81-4c0f-be6e-f69ec5162ea7

module CacheIO(
    -- cacheIO
    CacheIO,
    cacheIO,
    cacheIOeq,
    newCacheIO,
    MonadIO(..),
    JVar,
    modifyJVar,
    readJVar,
    waitJVar,
    waitJVarEq,
    waitJVarBy,
    newJVar,
    Readable(..),
    Writable(..),
    Token,
    waitJVarToken,
    badToken,
    combineVal

    ) where


import Control.Monad.Trans
import Control.Concurrent
import Monad
import System.Mem.StableName
import Control.Exception
import System.IO.Unsafe(unsafePerformIO)
import Data.IORef
--import IOExts

newtype CacheIO a = CacheIO { unCacheIO :: (IO (a, [IO Bool])) }


newCacheIO :: CacheIO a -> IO (IO a)
newCacheIO (CacheIO c) =  newMVar  (undefined, return False) >>= \mv -> return $ modifyMVar mv $ \z@(x,dep) -> do
    up <- dep
    if up then return (z,x) else c >>= \(x,deps) -> return ((x, (foldl (liftM2 (&&)) (return True) deps)),x)


cacheIO :: IO a -> CacheIO a
cacheIO io = CacheIO $ do
    v <- io
    sn <- makeStableName $! v
    let cc = do
        v <- io
        sn' <- makeStableName $! v
        return $ sn == sn'
    return (v,[cc])


cacheIOeq :: Eq a => IO a -> CacheIO a
cacheIOeq io = CacheIO $ do
    v <- io
    let cc = do
        v' <- io
        return $ v == v'
    return (v,[cc])

instance Functor CacheIO where
    fmap = liftM

instance Monad CacheIO where
    {-# INLINE (>>=) #-}
    {-# INLINE return #-}
    CacheIO a >>= b = CacheIO $ a >>= \(x,cv) -> unCacheIO (b x) >>= \(y,cv') -> return  (y,cv ++ cv')
    return a = CacheIO (return (a, []))

instance MonadIO CacheIO where
    {-# INLINE liftIO #-}
    liftIO a = CacheIO $ a >>= \x -> return (x,[])


newtype JVar a = JVar (MVar (a,[MVar ()]))
newtype Token a = Token (StableName a)

newJVar :: a -> IO (JVar a)
newJVar x = do
    mv <- newMVar (x,[])
    return $ JVar mv

modifyJVar :: JVar a -> (a -> IO (a,b)) -> IO b
modifyJVar (JVar mv) action = modifyMVar mv $ \(v,ws) -> do
    (nv, r) <- action v
    mapM_ (flip putMVar ()) ws
    return ((nv,[]),r)

readJVar :: JVar a -> IO a
readJVar (JVar mv) = readMVar mv >>= \(x,_) -> return x

badToken :: Token a
badToken = Token $ unsafePerformIO $ makeStableName myUndefined where
    myUndefined = myUndefined

waitJVarToken :: JVar a -> Token a -> IO (a,Token a)
waitJVarToken = undefined

waitJVarEq :: Eq a => JVar a -> a -> IO a
waitJVarEq = waitJVarBy (==)

waitJVarBy :: (a -> a -> Bool) -> JVar a -> a -> IO a
waitJVarBy eq jv@(JVar mv) a = do
    rv <- modifyMVar mv $ \v@(b,ws) -> if a `eq` b then do
            w <- newEmptyMVar
            return ((b,w:ws),Left w)
        else return (v,Right b)
    case rv of
        (Left w) -> takeMVar w >>= \() -> waitJVarBy eq jv a
        (Right v) -> return v


waitJVar :: JVar a -> a -> IO a
waitJVar jv@(JVar mv) a = do
    rv <- modifyMVar mv $ \v@(b,ws) -> do
        an <- makeStableName $! a
        bn <- makeStableName $! b
        if an == bn then do
            w <- newEmptyMVar
            return ((b,w:ws),Left w)
         else return (v,Right b)
    case rv of
        (Left w) -> takeMVar w >>= \() -> waitJVar  jv a
        (Right v) -> return v

instance Readable JVar where
    readVal x = liftIO $ readJVar x

instance Writable JVar where
    modifyVal a b = liftIO $ modifyJVar a b

instance Readable MVar where
    readVal x = liftIO $ readMVar x
instance Writable MVar where
    modifyVal a b = liftIO $ modifyMVar a b

class Readable c where
    readVal :: MonadIO m => c a -> m a

newtype ArbitraryReader a = ArbitraryReader (IO a)

instance Readable ArbitraryReader where
    readVal (ArbitraryReader x) = liftIO x

combineVal :: (Readable c1,Readable c2) => c1 a -> c2 b -> IO (ArbitraryReader (a,b))
combineVal sva svb = do
    let lsv = do
	av <- readVal sva
	bv <- readVal svb
        return (av,bv)
    return $ ArbitraryReader lsv

instance Readable IO where
    readVal = liftIO

class Readable c => Writable c where
    writeVal :: MonadIO m => c a -> a -> m ()
    swapVal :: MonadIO m => c a -> a -> m a
    modifyVal :: MonadIO m => c a -> (a -> IO (a,b)) -> m b
    modifyVal_ :: MonadIO m => c a -> (a -> IO a) -> m ()
    mapVal :: MonadIO m => c a -> (a -> a) -> m ()

    writeVal v x = swapVal v x >> return ()
    swapVal v x = modifyVal v $ \y -> return (x,y)
    modifyVal_ v action = modifyVal v $ \y -> action y >>= \x -> return (x,())
    mapVal v f = modifyVal_ v (return . f)


newtype StrictVar v a = StrictVar (v a)

instance Readable v => Readable (StrictVar v) where
    readVal (StrictVar  mv) = readVal mv

instance Writable v => Writable (StrictVar v) where
    modifyVal (StrictVar mv) f = modifyVal mv (\x -> f x >>= \(nx,r) -> evaluate nx >>= \nnx -> return (nnx,r))


instance Readable IORef where
    readVal x = liftIO $ readIORef x

instance Writable IORef where
    modifyVal ior f = liftIO $ do
        v <- readIORef ior
        (nv,r) <- f v
        writeIORef ior nv
        return r
