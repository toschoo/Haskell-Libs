module Paths_rake (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/ts/.cabal/bin"
libdir     = "/home/ts/.cabal/lib/rake-0.0.1/ghc-7.6.3"
datadir    = "/home/ts/.cabal/share/rake-0.0.1"
libexecdir = "/home/ts/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "rake_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rake_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rake_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rake_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
