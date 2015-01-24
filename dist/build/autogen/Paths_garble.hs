module Paths_garble (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bcarmer/.cabal/bin"
libdir     = "/home/bcarmer/.cabal/lib/x86_64-linux-ghc-7.8.4/garble-0.1.0.0"
datadir    = "/home/bcarmer/.cabal/share/x86_64-linux-ghc-7.8.4/garble-0.1.0.0"
libexecdir = "/home/bcarmer/.cabal/libexec"
sysconfdir = "/home/bcarmer/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "garble_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "garble_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "garble_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "garble_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "garble_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
