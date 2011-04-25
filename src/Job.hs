module Job where

import System.Directory
import System.Process
import System.FilePath

-- | need to be generalized
cabalInstallJob :: FilePath -> String -> IO () 
cabalInstallJob prog name = do 
  putStrLn $ "update : " ++  name
  setCurrentDirectory (prog </> name)
  system $ "cabal install"
  return () 
