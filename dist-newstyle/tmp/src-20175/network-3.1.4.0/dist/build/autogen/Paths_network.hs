{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_network (
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
version = Version [3,1,4,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.10.1-c219\\network-3.1.4.0-f0c31547215e1db6d516f1629c9419131ec688ef\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.10.1-c219\\network-3.1.4.0-f0c31547215e1db6d516f1629c9419131ec688ef\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.10.1-c219\\network-3.1.4.0-f0c31547215e1db6d516f1629c9419131ec688ef\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.10.1-c219\\network-3.1.4.0-f0c31547215e1db6d516f1629c9419131ec688ef\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.10.1-c219\\network-3.1.4.0-f0c31547215e1db6d516f1629c9419131ec688ef\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.10.1-c219\\network-3.1.4.0-f0c31547215e1db6d516f1629c9419131ec688ef\\etc"

getBinDir     = catchIO (getEnv "network_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "network_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "network_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "network_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "network_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "network_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
