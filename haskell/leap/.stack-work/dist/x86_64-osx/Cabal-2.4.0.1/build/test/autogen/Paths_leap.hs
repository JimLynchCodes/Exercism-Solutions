{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_leap (
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
version = Version [1,6,0,10] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jim/Exercism/haskell/leap/.stack-work/install/x86_64-osx/fb87fe1036c6f6c116bd9f6335b4e545ecc38b4bc3df3f555c887a350b725f41/8.6.5/bin"
libdir     = "/Users/jim/Exercism/haskell/leap/.stack-work/install/x86_64-osx/fb87fe1036c6f6c116bd9f6335b4e545ecc38b4bc3df3f555c887a350b725f41/8.6.5/lib/x86_64-osx-ghc-8.6.5/leap-1.6.0.10-6OP0yZMzXpuJf6xDnsgvWg-test"
dynlibdir  = "/Users/jim/Exercism/haskell/leap/.stack-work/install/x86_64-osx/fb87fe1036c6f6c116bd9f6335b4e545ecc38b4bc3df3f555c887a350b725f41/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/jim/Exercism/haskell/leap/.stack-work/install/x86_64-osx/fb87fe1036c6f6c116bd9f6335b4e545ecc38b4bc3df3f555c887a350b725f41/8.6.5/share/x86_64-osx-ghc-8.6.5/leap-1.6.0.10"
libexecdir = "/Users/jim/Exercism/haskell/leap/.stack-work/install/x86_64-osx/fb87fe1036c6f6c116bd9f6335b4e545ecc38b4bc3df3f555c887a350b725f41/8.6.5/libexec/x86_64-osx-ghc-8.6.5/leap-1.6.0.10"
sysconfdir = "/Users/jim/Exercism/haskell/leap/.stack-work/install/x86_64-osx/fb87fe1036c6f6c116bd9f6335b4e545ecc38b4bc3df3f555c887a350b725f41/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "leap_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "leap_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "leap_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "leap_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "leap_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "leap_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
