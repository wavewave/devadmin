module Main where

import System.FilePath 
import System.Environment
import Distribution.Verbosity
import Distribution.PackageDescription.Parse

import qualified Data.Map as M

import Data.Maybe
import Data.List 
import Data.Function


import Data.Graph.Inductive 


import Text.Parsec
import ParseConfig
import Project
import Graph
import Cabal
import Job

import Control.Applicative


main :: IO ()
main = do   
  args <- getArgs 
  homedir <- getEnv "HOME"
  putStrLn $ "build " ++ (args !! 0)
  putStrLn $ "reading " ++ (homedir </> ".build")
  configstr <- readFile (homedir </> ".build")
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right (p,w) -> do 
      gdescs <- mapM (readPackageDescription normal . getCabalFileName (p,w) ) projects 
      let deps = map ((,) <$> getPkgName <*> getDependency) gdescs
          motherlist = map ((,) <$> fst  <*> (filter (nameMatch projects). snd)) deps
          daughtermap = convertMotherMapToDaughterMap motherlist 
          daughterlist = M.toList daughtermap 
          edgelist = concatMap  mkDepEdge daughterlist
          allnodes = idproj 
          gr :: Gr String () 
          gr = mkGraph allnodes edgelist
          linear = topsort gr  
          strlst = map (\x->fromJust $ M.lookup x idprojmap) linear 
     
      let alldaughters = nub $ findAllDaughters daughtermap  (args !! 0)
          numbered = map (\x -> findOrder x strlst) alldaughters 
          finallist = map snd . sortBy (compare `on` fst) $ numbered 

      mapM_ (cabalInstallJob p) finallist 

--      putStrLn $ show finallist
--      putStrLn (show daughterlist ) 
--      putStrLn . show $ nub $   
--       mapM_ (cabalInstallJob p)  r 


    