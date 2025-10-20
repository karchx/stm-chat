{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_stm_chat (
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
version = Version [0,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/stivarch/src/projects/stm-chat/.stack-work/install/x86_64-linux/25ccd4bdfe033c5496d1bbb95910cfeef5ffae170767acf428858749502e766c/9.10.3/bin"
libdir     = "/home/stivarch/src/projects/stm-chat/.stack-work/install/x86_64-linux/25ccd4bdfe033c5496d1bbb95910cfeef5ffae170767acf428858749502e766c/9.10.3/lib/x86_64-linux-ghc-9.10.3-415c/stm-chat-0.0.0-KwjhYpyE15dBY2TQMfNA47-render"
dynlibdir  = "/home/stivarch/src/projects/stm-chat/.stack-work/install/x86_64-linux/25ccd4bdfe033c5496d1bbb95910cfeef5ffae170767acf428858749502e766c/9.10.3/lib/x86_64-linux-ghc-9.10.3-415c"
datadir    = "/home/stivarch/src/projects/stm-chat/.stack-work/install/x86_64-linux/25ccd4bdfe033c5496d1bbb95910cfeef5ffae170767acf428858749502e766c/9.10.3/share/x86_64-linux-ghc-9.10.3-415c/stm-chat-0.0.0"
libexecdir = "/home/stivarch/src/projects/stm-chat/.stack-work/install/x86_64-linux/25ccd4bdfe033c5496d1bbb95910cfeef5ffae170767acf428858749502e766c/9.10.3/libexec/x86_64-linux-ghc-9.10.3-415c/stm-chat-0.0.0"
sysconfdir = "/home/stivarch/src/projects/stm-chat/.stack-work/install/x86_64-linux/25ccd4bdfe033c5496d1bbb95910cfeef5ffae170767acf428858749502e766c/9.10.3/etc"

getBinDir     = catchIO (getEnv "stm_chat_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "stm_chat_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "stm_chat_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "stm_chat_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stm_chat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stm_chat_sysconfdir") (\_ -> return sysconfdir)



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
