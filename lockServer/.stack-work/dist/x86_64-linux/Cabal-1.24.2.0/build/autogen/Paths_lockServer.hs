{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_lockServer (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/protoman/Documents/CS4532/DFS/lockServer/.stack-work/install/x86_64-linux/lts-7.15/8.0.1/bin"
libdir     = "/home/protoman/Documents/CS4532/DFS/lockServer/.stack-work/install/x86_64-linux/lts-7.15/8.0.1/lib/x86_64-linux-ghc-8.0.1/lockServer-0.1.0.0-7PbJ57tJ75PIb86aqn1tPz"
dynlibdir  = "/home/protoman/Documents/CS4532/DFS/lockServer/.stack-work/install/x86_64-linux/lts-7.15/8.0.1/lib/x86_64-linux-ghc-8.0.1"
datadir    = "/home/protoman/Documents/CS4532/DFS/lockServer/.stack-work/install/x86_64-linux/lts-7.15/8.0.1/share/x86_64-linux-ghc-8.0.1/lockServer-0.1.0.0"
libexecdir = "/home/protoman/Documents/CS4532/DFS/lockServer/.stack-work/install/x86_64-linux/lts-7.15/8.0.1/libexec"
sysconfdir = "/home/protoman/Documents/CS4532/DFS/lockServer/.stack-work/install/x86_64-linux/lts-7.15/8.0.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lockServer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lockServer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lockServer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lockServer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lockServer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lockServer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
