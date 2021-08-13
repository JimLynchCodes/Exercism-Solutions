{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_space_age (
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
version = Version [1,2,0,6] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jim/Exercism/haskell/space-age/.stack-work/install/x86_64-osx/a59363cecdb5ed60c738f4f993aeb7c5e9ecf7875f8609d8dbbaaa89e35e299f/8.8.4/bin"
libdir     = "/Users/jim/Exercism/haskell/space-age/.stack-work/install/x86_64-osx/a59363cecdb5ed60c738f4f993aeb7c5e9ecf7875f8609d8dbbaaa89e35e299f/8.8.4/lib/x86_64-osx-ghc-8.8.4/space-age-1.2.0.6-ERGMq6vZElxEToe7vqaQth"
dynlibdir  = "/Users/jim/Exercism/haskell/space-age/.stack-work/install/x86_64-osx/a59363cecdb5ed60c738f4f993aeb7c5e9ecf7875f8609d8dbbaaa89e35e299f/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/jim/Exercism/haskell/space-age/.stack-work/install/x86_64-osx/a59363cecdb5ed60c738f4f993aeb7c5e9ecf7875f8609d8dbbaaa89e35e299f/8.8.4/share/x86_64-osx-ghc-8.8.4/space-age-1.2.0.6"
libexecdir = "/Users/jim/Exercism/haskell/space-age/.stack-work/install/x86_64-osx/a59363cecdb5ed60c738f4f993aeb7c5e9ecf7875f8609d8dbbaaa89e35e299f/8.8.4/libexec/x86_64-osx-ghc-8.8.4/space-age-1.2.0.6"
sysconfdir = "/Users/jim/Exercism/haskell/space-age/.stack-work/install/x86_64-osx/a59363cecdb5ed60c738f4f993aeb7c5e9ecf7875f8609d8dbbaaa89e35e299f/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "space_age_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "space_age_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "space_age_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "space_age_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "space_age_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "space_age_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
