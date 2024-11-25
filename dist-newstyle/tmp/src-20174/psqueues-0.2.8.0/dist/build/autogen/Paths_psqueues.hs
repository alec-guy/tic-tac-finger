{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_psqueues (
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
version = Version [0,2,8,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.10.1-c219\\psqueues-0.2.8.0-8aff352685fb7a41ee564eed541b260ddb268707\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.10.1-c219\\psqueues-0.2.8.0-8aff352685fb7a41ee564eed541b260ddb268707\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.10.1-c219\\psqueues-0.2.8.0-8aff352685fb7a41ee564eed541b260ddb268707\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.10.1-c219\\psqueues-0.2.8.0-8aff352685fb7a41ee564eed541b260ddb268707\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.10.1-c219\\psqueues-0.2.8.0-8aff352685fb7a41ee564eed541b260ddb268707\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.10.1-c219\\psqueues-0.2.8.0-8aff352685fb7a41ee564eed541b260ddb268707\\etc"

getBinDir     = catchIO (getEnv "psqueues_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "psqueues_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "psqueues_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "psqueues_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "psqueues_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "psqueues_sysconfdir") (\_ -> return sysconfdir)



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
