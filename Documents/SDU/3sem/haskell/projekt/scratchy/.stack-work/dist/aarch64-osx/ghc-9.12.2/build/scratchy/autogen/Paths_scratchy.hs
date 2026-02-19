{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_scratchy (
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
bindir     = "/Users/simonholm/examen obsidian/SDU/3sem/haskell/projekt/.stack-work/install/aarch64-osx/2890cab57224a82b2f7d9caa4963e242425431ffe80fc3b0f98988038e28b7b3/9.12.2/bin"
libdir     = "/Users/simonholm/examen obsidian/SDU/3sem/haskell/projekt/.stack-work/install/aarch64-osx/2890cab57224a82b2f7d9caa4963e242425431ffe80fc3b0f98988038e28b7b3/9.12.2/lib/aarch64-osx-ghc-9.12.2-ea3d/scratchy-0.1.0.0-FYis9qCFFgcBsZ26jG1mi2-scratchy"
dynlibdir  = "/Users/simonholm/examen obsidian/SDU/3sem/haskell/projekt/.stack-work/install/aarch64-osx/2890cab57224a82b2f7d9caa4963e242425431ffe80fc3b0f98988038e28b7b3/9.12.2/lib/aarch64-osx-ghc-9.12.2-ea3d"
datadir    = "/Users/simonholm/examen obsidian/SDU/3sem/haskell/projekt/.stack-work/install/aarch64-osx/2890cab57224a82b2f7d9caa4963e242425431ffe80fc3b0f98988038e28b7b3/9.12.2/share/aarch64-osx-ghc-9.12.2-ea3d/scratchy-0.1.0.0"
libexecdir = "/Users/simonholm/examen obsidian/SDU/3sem/haskell/projekt/.stack-work/install/aarch64-osx/2890cab57224a82b2f7d9caa4963e242425431ffe80fc3b0f98988038e28b7b3/9.12.2/libexec/aarch64-osx-ghc-9.12.2-ea3d/scratchy-0.1.0.0"
sysconfdir = "/Users/simonholm/examen obsidian/SDU/3sem/haskell/projekt/.stack-work/install/aarch64-osx/2890cab57224a82b2f7d9caa4963e242425431ffe80fc3b0f98988038e28b7b3/9.12.2/etc"

getBinDir     = catchIO (getEnv "scratchy_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "scratchy_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "scratchy_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "scratchy_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scratchy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scratchy_sysconfdir") (\_ -> return sysconfdir)



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
