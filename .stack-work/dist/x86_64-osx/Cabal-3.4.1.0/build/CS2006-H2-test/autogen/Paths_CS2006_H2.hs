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

bindir     = "/Users/jimmyzhang/Documents/CS2006/Practicals/CS2006-H2/.stack-work/install/x86_64-osx/ba5f5ccb4ba602eabcf275b43fb2a6728a213e076030915b3cebc03cae996626/9.0.2/bin"
libdir     = "/Users/jimmyzhang/Documents/CS2006/Practicals/CS2006-H2/.stack-work/install/x86_64-osx/ba5f5ccb4ba602eabcf275b43fb2a6728a213e076030915b3cebc03cae996626/9.0.2/lib/x86_64-osx-ghc-9.0.2/CS2006-H2-0.1.0.0-DN9XVNtaqUEIYyRaRHYd1R-CS2006-H2-test"
dynlibdir  = "/Users/jimmyzhang/Documents/CS2006/Practicals/CS2006-H2/.stack-work/install/x86_64-osx/ba5f5ccb4ba602eabcf275b43fb2a6728a213e076030915b3cebc03cae996626/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/jimmyzhang/Documents/CS2006/Practicals/CS2006-H2/.stack-work/install/x86_64-osx/ba5f5ccb4ba602eabcf275b43fb2a6728a213e076030915b3cebc03cae996626/9.0.2/share/x86_64-osx-ghc-9.0.2/CS2006-H2-0.1.0.0"
libexecdir = "/Users/jimmyzhang/Documents/CS2006/Practicals/CS2006-H2/.stack-work/install/x86_64-osx/ba5f5ccb4ba602eabcf275b43fb2a6728a213e076030915b3cebc03cae996626/9.0.2/libexec/x86_64-osx-ghc-9.0.2/CS2006-H2-0.1.0.0"
sysconfdir = "/Users/jimmyzhang/Documents/CS2006/Practicals/CS2006-H2/.stack-work/install/x86_64-osx/ba5f5ccb4ba602eabcf275b43fb2a6728a213e076030915b3cebc03cae996626/9.0.2/etc"

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
  return (dir ++ "/" ++ name)
