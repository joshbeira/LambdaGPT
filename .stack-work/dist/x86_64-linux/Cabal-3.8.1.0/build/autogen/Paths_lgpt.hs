{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lgpt (
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
bindir     = "/dcs/25/u5683397/Documents/cs141/cw2/lgpt/.stack-work/install/x86_64-linux/2fcfeb28e342e22c83a6946cc3264c9633761ec0918683892cadfdcbb79e35d9/9.4.8/bin"
libdir     = "/dcs/25/u5683397/Documents/cs141/cw2/lgpt/.stack-work/install/x86_64-linux/2fcfeb28e342e22c83a6946cc3264c9633761ec0918683892cadfdcbb79e35d9/9.4.8/lib/x86_64-linux-ghc-9.4.8/lgpt-0.1.0.0-E3BZpRly2K0ICXJjsDpzs3"
dynlibdir  = "/dcs/25/u5683397/Documents/cs141/cw2/lgpt/.stack-work/install/x86_64-linux/2fcfeb28e342e22c83a6946cc3264c9633761ec0918683892cadfdcbb79e35d9/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/dcs/25/u5683397/Documents/cs141/cw2/lgpt/.stack-work/install/x86_64-linux/2fcfeb28e342e22c83a6946cc3264c9633761ec0918683892cadfdcbb79e35d9/9.4.8/share/x86_64-linux-ghc-9.4.8/lgpt-0.1.0.0"
libexecdir = "/dcs/25/u5683397/Documents/cs141/cw2/lgpt/.stack-work/install/x86_64-linux/2fcfeb28e342e22c83a6946cc3264c9633761ec0918683892cadfdcbb79e35d9/9.4.8/libexec/x86_64-linux-ghc-9.4.8/lgpt-0.1.0.0"
sysconfdir = "/dcs/25/u5683397/Documents/cs141/cw2/lgpt/.stack-work/install/x86_64-linux/2fcfeb28e342e22c83a6946cc3264c9633761ec0918683892cadfdcbb79e35d9/9.4.8/etc"

getBinDir     = catchIO (getEnv "lgpt_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lgpt_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lgpt_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lgpt_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lgpt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lgpt_sysconfdir") (\_ -> return sysconfdir)




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
