module Paths_vxml (
	version,
	getBinDir, getLibDir, getDataDir, getLibexecDir,
	getDataFileName
	) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/vxml-0.0/ghc-6.8.2"
datadir    = "/usr/local/share/vxml-0.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "vxml_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "vxml_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "vxml_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "vxml_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
