module Directories ((+/+), dirname, basename, makeDirectory)
where

import System.Directory (createDirectory, doesDirectoryExist)

(+/+) :: String -> String -> String
(+/+) s1 s2 =
    (if last s1 == '/' then s1 else s1 ++ "/") ++
    (if (not . null ) s2 && head s2 == '/' then tail s2 else s2)

dirname :: FilePath -> FilePath
dirname = joinSegments . init . pathSegments

basename :: FilePath -> FilePath
basename = last . pathSegments

pathSegments :: FilePath -> [String]
pathSegments fs | last fs == '/' = pathSegments $ init fs
pathSegments fs =
    let (l, s') = break (== '/') fs in
      (l) : case s' of
	      []      -> []
	      (_:s'') -> pathSegments s''

joinSegments :: [String] -> FilePath
joinSegments = concatMap (++ "/")

makeDirectory :: FilePath -> IO ()
makeDirectory dir = do
    exists <- doesDirectoryExist dir
    if exists
       then return ()
       else putStrLn ("creating " ++ dir) >> createDirectory dir
