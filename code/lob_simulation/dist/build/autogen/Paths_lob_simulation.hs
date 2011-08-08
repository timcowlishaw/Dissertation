module Paths_lob_simulation (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tim/Documents/MSc/DISSERTATION/code/lob_simulation/cabal-dev//bin"
libdir     = "/home/tim/Documents/MSc/DISSERTATION/code/lob_simulation/cabal-dev//lib/lob-simulation-0.1/ghc-6.12.3"
datadir    = "/home/tim/Documents/MSc/DISSERTATION/code/lob_simulation/cabal-dev//share/lob-simulation-0.1"
libexecdir = "/home/tim/Documents/MSc/DISSERTATION/code/lob_simulation/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "lob_simulation_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "lob_simulation_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "lob_simulation_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "lob_simulation_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
