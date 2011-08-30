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

import Application.DevAdmin.Config
import Application.DevAdmin.Project
import Application.DevAdmin.Graph
import Application.DevAdmin.Cabal
import Application.DevAdmin.Job

import Control.Applicative
import Control.Monad


makeProjDepOrderList :: BuildConfiguration -> IO (DaughterMap,[String])
makeProjDepOrderList bc = do 
  putStrLn $ show bc
  let (p,w) = (,) <$> bc_progbase <*> bc_workspacebase $ bc
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
  return (daughtermap,strlst)

makeProjDepList :: BuildConfiguration -> [String] -> IO [String]
makeProjDepList bc projs = do 
  (daughtermap,strlst) <- makeProjDepOrderList bc 
     
  let alldaughters = nub . concatMap (findAllDaughters daughtermap) $ projs
      numbered = map (\x -> findOrder x strlst) alldaughters 
      finallist = map snd . sortBy (compare `on` fst) $ numbered 
  return finallist

main :: IO ()
main = do   
  args <- getArgs 
  when (length args /= 2) $ error "2 arguments needed"
  homedir <- getEnv "HOME"
  putStrLn $ "build " ++ (args !! 0) ++ " " ++ (args !! 1) 
  putStrLn $ "reading " ++ (homedir </> ".build")
  configstr <- readFile (homedir </> ".build")
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right bc -> do
      let projlist = map projname projects 
      finallist <- makeProjDepList bc [args !! 1]
      allorderedlist <- makeProjDepList bc (map projname projects)
--      putStrLn $ show projlist
--      putStrLn $ show finallist
--      putStrLn $ show allorderedlist 

      case (args !! 0) of 
        "install" -> flip mapM_ finallist (cabalInstallJob  bc)
        "push"    -> flip mapM_ allorderedlist  (darcsPushJob     bc)
        "haddock" -> flip mapM_ finallist (haddockJob       bc)
        "depshow" -> flip mapM_ finallist (depshowJob       bc)
        "pull"    -> do 
          putStrLn $ "allorderedlist = " ++  (show allorderedlist)
          flip mapM_ allorderedlist (darcsPullJob     bc)
        "hoogle"  -> flip mapM_ finallist (hoogleJob        bc)
        "whatsnew" -> flip mapM_ allorderedlist (darcsWhatsnewJob bc)
        "bootstrap" -> flip mapM_ allorderedlist (cabalInstallJob bc)
        _ -> error "no such option"


    