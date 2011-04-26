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


darcsPushJob :: FilePath -> String -> IO () 
darcsPushJob prog name = do 
  putStrLn $ "darcs push : " ++  name
  setCurrentDirectory (prog </> name)
  system $ "darcs push"
  return () 

haddockJob :: FilePath -> String -> IO () 
haddockJob prog name = do 
  putStrLn $ "haddock : " ++ name 
  setCurrentDirectory (prog </> name)
  system $ "./haddock"
  return () 
