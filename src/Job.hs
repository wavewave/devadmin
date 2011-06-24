module Job where

import System.Directory
import System.Process
import System.FilePath

import Config
import VersionCheck

depshowJob :: BuildConfiguration -> String -> IO () 
depshowJob bc name = do 
   putStrLn $ "currently working on " ++ name 



-- | need to be generalized
cabalInstallJob :: BuildConfiguration -> String -> IO () 
cabalInstallJob bc name = do 
  putStrLn $ "update : " ++  name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "cabal install"
  return () 


darcsPushJob :: BuildConfiguration -> String -> IO () 
darcsPushJob bc name = do 
  putStrLn $ "darcs push : " ++  name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "darcs push"
  return () 

haddockJob :: BuildConfiguration -> String -> IO () 
haddockJob bc name = do 
  putStrLn $ "haddock : " ++ name 
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "cabal install --enable-documentation"
  system $ "cabal haddock --hyperlink-source"
  system $ "cabal install"
  versioncheck bc
  return () 
