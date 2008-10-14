{-# OPTIONS_GHC -fallow-overlapping-instances #-}

module Status(
    set,
    setS,
    clear,
    Status.get,
    setF,
    setFS,
    getStatus,
    Status.log,
    getLog
    ) where


import Char(chr)
import CircularBuffer as CB
import Data.IORef
import Data.Tree
import Doc.Chars
import GenUtil
import List(intersperse,groupBy)
import qualified Data.Map as Map
import System.IO.Unsafe


{-# NOINLINE status_var #-}
status_var :: IORef (Map.Map String (IO String))
status_var  = unsafePerformIO $ newIORef Map.empty

{-# NOINLINE log_var #-}
log_var :: CB.CircularBuffer String
log_var = unsafePerformIO $ CB.new 10

log :: String -> IO ()
log s = CB.append log_var [s]

getLog :: IO [String]
getLog = CB.toList log_var



modify r f = atomicModifyIORef r (\x -> (f x,()))

setS :: Show a => String -> a -> IO ()
setS w v = set w (show v)

set :: String -> String -> IO ()
set w v = modify status_var (\fm -> Map.insert w (return v) fm)

setF :: String -> IO String -> IO ()
setF w v = modify status_var (\fm -> Map.insert w v fm)

setFS :: Show a => String -> IO a -> IO ()
setFS w v = modify status_var (\fm -> Map.insert w (fmap show v) fm)


get :: String -> IO (IO String)
get k = do
    fm <- readIORef status_var
    case Map.lookup k fm of
        Just x -> return x
        Nothing -> return (return "")



clear :: String -> IO ()
clear k =  modify status_var (\fm -> Map.delete k fm)


getall = do
    fm <- readIORef status_var
    return $ Map.toList fm

getTree :: IO (Forest (String,String))
getTree = do
    xs <- getall
    let f (a,b) = do b <- b; return (split (== '.') a,b)
    xs <- mapM f xs
    return $ createForest  xs

createForest  xs = map f gs where
    --[Node (concat $ intersperse "." (xs),y) [] | (xs,y) <- xs]
    f [(xs,ys)] =  Node (concat $ intersperse "." (xs),ys) []
    f xs@((x:_,_):_) = Node (x,"") (createForest [ (xs,ys) | (_:xs,ys)<- xs])
    f _ = error "createForest: should not happen."
    gs = groupBy (\(x:_,_) (y:_,_) -> x == y) xs
--createForest  xs = Node ("","") [ createTree [(xs,y)] | (xs,y) <- xs]

draw :: Tree String -> [String]
draw (Node x ts0) = x : drawSubTrees ts0
  where drawSubTrees [] = []
        drawSubTrees [t] =
                {-[vLine] :-} shift [chr 0x2570, chr 0x2574] "  " (draw t)
        drawSubTrees (t:ts) =
                {-[vLine] :-} shift (lTee ++ [chr 0x2574]) (vLine ++  " ") (draw t) ++ drawSubTrees ts

        shift first other = zipWith (++) (first : repeat other)
        --vLine = chr 0x254F

getStatus :: IO String
getStatus = do
    t <- getTree
    let f (xs,"") = xs
        f (xs,ys) = xs ++ ": "  ++ ys
    return $ unlines (concatMap (draw . fmap f) t)




