module Job where

import System.Directory
import System.Process
import System.FilePath

depshowJob :: FilePath -> String -> IO () 
depshowJob prog name = do 
   putStrLn $ "currently working on " ++ name 



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


darcsPullJob :: FilePath -> String -> IO () 
darcsPullJob prog name = do 
  putStrLn $ "darcs pull : " ++  name
  setCurrentDirectory (prog </> name)
  system $ "darcs pull"
  return () 

haddockJob :: FilePath -> String -> IO () 
haddockJob prog name = do 
  putStrLn $ "haddock : " ++ name 
  setCurrentDirectory (prog </> name)
  system $ "sh ./haddock"
  return () 
