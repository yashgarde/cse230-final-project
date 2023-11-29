{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cse230_final_project (
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
bindir     = "/home/codespace/.cabal/bin"
libdir     = "/home/codespace/.cabal/lib/x86_64-linux-ghc-9.4.7/cse230-final-project-0.1.0.0-inplace"
dynlibdir  = "/home/codespace/.cabal/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/codespace/.cabal/share/x86_64-linux-ghc-9.4.7/cse230-final-project-0.1.0.0"
libexecdir = "/home/codespace/.cabal/libexec/x86_64-linux-ghc-9.4.7/cse230-final-project-0.1.0.0"
sysconfdir = "/home/codespace/.cabal/etc"

getBinDir     = catchIO (getEnv "cse230_final_project_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cse230_final_project_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cse230_final_project_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cse230_final_project_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cse230_final_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cse230_final_project_sysconfdir") (\_ -> return sysconfdir)




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
