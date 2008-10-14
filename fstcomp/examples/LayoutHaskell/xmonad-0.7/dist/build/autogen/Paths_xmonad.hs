module Paths_xmonad (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version

version :: Version
version = Version {versionBranch = [0,7], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/clstaff/apel/bin"
libdir     = "/home/clstaff/apel/lib/xmonad-0.7/ghc-6.8.3"
datadir    = "/home/clstaff/apel/share/xmonad-0.7"
libexecdir = "/home/clstaff/apel/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = return bindir
getLibDir = return libdir
getDataDir = return datadir
getLibexecDir = return libexecdir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = return (datadir ++ "/" ++ name)
