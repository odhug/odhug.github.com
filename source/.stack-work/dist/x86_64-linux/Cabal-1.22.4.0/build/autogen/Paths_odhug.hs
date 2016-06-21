module Paths_odhug (
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
version = Version [0,9,1,15] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sigrlami/work/projects-www/odhug.github.com/source/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/bin"
libdir     = "/home/sigrlami/work/projects-www/odhug.github.com/source/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/lib/x86_64-linux-ghc-7.10.2/odhug-0.9.1.15-H8Vv4bQVprz4m6Xvzb0W6d"
datadir    = "/home/sigrlami/work/projects-www/odhug.github.com/source/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/share/x86_64-linux-ghc-7.10.2/odhug-0.9.1.15"
libexecdir = "/home/sigrlami/work/projects-www/odhug.github.com/source/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/libexec"
sysconfdir = "/home/sigrlami/work/projects-www/odhug.github.com/source/.stack-work/install/x86_64-linux/lts-3.8/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "odhug_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "odhug_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "odhug_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "odhug_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "odhug_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
