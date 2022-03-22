{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_CS2006_H2 (
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

bindir     = "C:\\Users\\user\\Documents\\Program\\Year2\\CS2006\\H2\\.stack-work\\install\\a37b3e72\\bin"
libdir     = "C:\\Users\\user\\Documents\\Program\\Year2\\CS2006\\H2\\.stack-work\\install\\a37b3e72\\lib\\x86_64-windows-ghc-9.0.2\\CS2006-H2-0.1.0.0-BIB1yB1unQ3EDVDtsIsrlo"
dynlibdir  = "C:\\Users\\user\\Documents\\Program\\Year2\\CS2006\\H2\\.stack-work\\install\\a37b3e72\\lib\\x86_64-windows-ghc-9.0.2"
datadir    = "C:\\Users\\user\\Documents\\Program\\Year2\\CS2006\\H2\\.stack-work\\install\\a37b3e72\\share\\x86_64-windows-ghc-9.0.2\\CS2006-H2-0.1.0.0"
libexecdir = "C:\\Users\\user\\Documents\\Program\\Year2\\CS2006\\H2\\.stack-work\\install\\a37b3e72\\libexec\\x86_64-windows-ghc-9.0.2\\CS2006-H2-0.1.0.0"
sysconfdir = "C:\\Users\\user\\Documents\\Program\\Year2\\CS2006\\H2\\.stack-work\\install\\a37b3e72\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "CS2006_H2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "CS2006_H2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "CS2006_H2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "CS2006_H2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "CS2006_H2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "CS2006_H2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
