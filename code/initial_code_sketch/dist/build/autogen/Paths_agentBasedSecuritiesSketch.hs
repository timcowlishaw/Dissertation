module Paths_agentBasedSecuritiesSketch (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/tim/Documents/MSc/DISSERTATION/code/initial_code_sketch/cabal-dev//bin"
libdir     = "/home/tim/Documents/MSc/DISSERTATION/code/initial_code_sketch/cabal-dev//lib/agentBasedSecuritiesSketch-0.1/ghc-6.12.3"
datadir    = "/home/tim/Documents/MSc/DISSERTATION/code/initial_code_sketch/cabal-dev//share/agentBasedSecuritiesSketch-0.1"
libexecdir = "/home/tim/Documents/MSc/DISSERTATION/code/initial_code_sketch/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "agentBasedSecuritiesSketch_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "agentBasedSecuritiesSketch_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "agentBasedSecuritiesSketch_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "agentBasedSecuritiesSketch_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
