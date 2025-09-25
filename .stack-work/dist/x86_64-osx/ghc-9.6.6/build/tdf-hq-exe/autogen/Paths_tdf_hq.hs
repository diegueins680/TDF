{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tdf_hq (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/diegosaa/Documents/GitHub/tdf-app/tdf-hq/.stack-work/install/x86_64-osx/fc42097e877d470571a1f395ed2521825b002f3ede4a1e563fc1a776c1284694/9.6.6/bin"
libdir     = "/Users/diegosaa/Documents/GitHub/tdf-app/tdf-hq/.stack-work/install/x86_64-osx/fc42097e877d470571a1f395ed2521825b002f3ede4a1e563fc1a776c1284694/9.6.6/lib/x86_64-osx-ghc-9.6.6/tdf-hq-0.1.0.0-8ODKnb9tw1U42UaiXsupn9-tdf-hq-exe"
dynlibdir  = "/Users/diegosaa/Documents/GitHub/tdf-app/tdf-hq/.stack-work/install/x86_64-osx/fc42097e877d470571a1f395ed2521825b002f3ede4a1e563fc1a776c1284694/9.6.6/lib/x86_64-osx-ghc-9.6.6"
datadir    = "/Users/diegosaa/Documents/GitHub/tdf-app/tdf-hq/.stack-work/install/x86_64-osx/fc42097e877d470571a1f395ed2521825b002f3ede4a1e563fc1a776c1284694/9.6.6/share/x86_64-osx-ghc-9.6.6/tdf-hq-0.1.0.0"
libexecdir = "/Users/diegosaa/Documents/GitHub/tdf-app/tdf-hq/.stack-work/install/x86_64-osx/fc42097e877d470571a1f395ed2521825b002f3ede4a1e563fc1a776c1284694/9.6.6/libexec/x86_64-osx-ghc-9.6.6/tdf-hq-0.1.0.0"
sysconfdir = "/Users/diegosaa/Documents/GitHub/tdf-app/tdf-hq/.stack-work/install/x86_64-osx/fc42097e877d470571a1f395ed2521825b002f3ede4a1e563fc1a776c1284694/9.6.6/etc"

getBinDir     = catchIO (getEnv "tdf_hq_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tdf_hq_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tdf_hq_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tdf_hq_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tdf_hq_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tdf_hq_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
