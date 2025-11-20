{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_tdf_hq (
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

bindir     = "/Users/diegosaa/.cabal/bin"
libdir     = "/Users/diegosaa/.cabal/lib/x86_64-osx-ghc-8.10.4/tdf-hq-0.1.0.0-inplace-tdf-hq-exe"
dynlibdir  = "/Users/diegosaa/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/diegosaa/.cabal/share/x86_64-osx-ghc-8.10.4/tdf-hq-0.1.0.0"
libexecdir = "/Users/diegosaa/.cabal/libexec/x86_64-osx-ghc-8.10.4/tdf-hq-0.1.0.0"
sysconfdir = "/Users/diegosaa/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tdf_hq_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tdf_hq_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tdf_hq_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tdf_hq_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tdf_hq_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tdf_hq_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
