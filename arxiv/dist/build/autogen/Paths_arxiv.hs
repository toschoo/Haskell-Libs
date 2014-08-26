module Paths_arxiv (
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
libdir     = "/home/ts/.cabal/lib/arxiv-0.0.1/ghc-7.6.3"
datadir    = "/home/ts/.cabal/share/arxiv-0.0.1"
libexecdir = "/home/ts/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "arxiv_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "arxiv_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "arxiv_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "arxiv_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
