{-# OPTIONS -fglasgow-exts #-}
module CircularBuffer(
    CircularBuffer,
    new,
    get,
    append,
    toList,
    CircularBuffer.length
    ) where

import Control.Concurrent
import Data.Array.IO

data Meta = Full {-# UNPACK #-} !Int | Partial {-# UNPACK #-} !Int
data CircularBuffer e = CB { arr :: !(IOArray Int e), mvar :: !(MVar Meta)}
--    FullCB    { start :: !Int, arr ::  }
--    | PartialCB { len :: !Int, arr :: !(IOArray Int e) }

new :: Int -> IO (CircularBuffer a)
new s | s < 1 = fail "cannot create that small of a circular buffer"
new s = do
    a <- newArray_ (0,s - 1)
    m <- newMVar $ Partial 0
    return CB { mvar = m,  arr = a }

get :: CircularBuffer a -> Int -> IO a
get CB { arr = arr, mvar = mvar} w = withMVar mvar go where
    go (Partial 0) = fail "attempt to read empty CircularBuffer"
    go (Partial len) = do
        let m = w `mod` len
            w' = if m < 0 then m + len else m
        readArray arr w'
    go (Full start) = do
        bnds <- getBounds arr
        let m = (w + start) `mod` len
            w' = if m < 0 then m + len else m
            len = snd bnds + 1
        readArray arr w'

length :: CircularBuffer a -> IO Int
length CB { arr = arr, mvar = mvar} = withMVar mvar go where
    go (Full _) = do
        bnds <- getBounds arr
        return $ (snd bnds) + 1
    go (Partial len) = return len

append :: CircularBuffer a -> [a] -> IO ()
append CB {arr = arr, mvar = mvar} xs = do
    bnds <- getBounds arr
    let alen = snd bnds + 1
        xslen = Prelude.length xs
        go _ | xslen >= alen = do
            sequence_ [writeArray arr i e | i <- [0..] | e <- (drop $ xslen - alen) xs  ]
            return (Full 0)
        go (Full start) = do
            let nstart = (start + xslen) `mod` alen
            sequence_ [writeArray arr (i `mod` alen) e | i <- [start..] | e <-  xs  ]
            return (Full nstart)
        go (Partial len) | len + xslen ==  alen = do
            sequence_ [writeArray arr i e | i <- [len..] | e <-  xs  ]
            return (Full 0)
        go (Partial len) | nlen <- len + xslen, nlen < alen = do
            sequence_ [writeArray arr i e | i <- [len..] | e <-  xs  ]
            return (Partial nlen)
        go (Partial len)  = do
            sequence_ [writeArray arr (i `mod` alen) e | i <- [len..] | e <-  xs  ]
            return (Full (xslen + len - alen))
    modifyMVar_ mvar go



--prepend :: CircularBuffer a -> [a] -> IO ()

toList :: CircularBuffer a -> IO [a]
toList CB { arr = arr, mvar = mvar} = withMVar mvar go where
    go (Partial len) = do
        es <- getElems arr
        return (take len es)
    go (Full start) = do
        es <- getElems arr
        let (a,b) = splitAt start es
        return (b ++ a)

--first cb = read cb 0
--last cb = read cb -1

