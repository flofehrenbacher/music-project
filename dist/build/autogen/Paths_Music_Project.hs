module Paths_Music_Project (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Florian\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Florian\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.3\\Music-Project-0.1.0.0-7ITDYYykJrqCvHKSMisyAZ"
datadir    = "C:\\Users\\Florian\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.3\\Music-Project-0.1.0.0"
libexecdir = "C:\\Users\\Florian\\AppData\\Roaming\\cabal\\Music-Project-0.1.0.0-7ITDYYykJrqCvHKSMisyAZ"
sysconfdir = "C:\\Users\\Florian\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Music_Project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Music_Project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Music_Project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Music_Project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Music_Project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
