module Main where

import System.FilePath 
import System.IO
import System.Environment
import System.Process
import System.Directory 

import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

import qualified Data.Map as M

import Data.Maybe

prog = "/home/wavewave/nfs/prog" 
workspace = "/home/wavewave/nfs/workspace"

-- projects = [ "ttbar" , "dmmcoll2" ] 
-- workspace

data Project = WorkspaceProj { workspacename :: String, projname :: String } 
             | ProgProj { projname :: String } 

projects = [ ProgProj "LHCOAnalysis" 
           , ProgProj "LHCOAnalysis-type" 
           , ProgProj "iteratee-util" 
           , ProgProj "pipeline"
           , ProgProj "madgraph-auto"
           , ProgProj "madgraph-auto-model"
           , ProgProj "madgraph-auto-dataset"
           , ProgProj "HROOT-generate"
           , ProgProj "HROOT"
           , ProgProj "MSSMType"
           , ProgProj "MSSMScan"
           , ProgProj "MSSMPlot" 
           , ProgProj "LHEParser"
           , ProgProj "simann"
           , ProgProj "HEPMonteCarlo"
           , ProgProj "ttbar"
           , ProgProj "HEPUtil"
           , ProgProj "iteratee-util"
           , ProgProj "HStringTemplateHelpersIW"
           , ProgProj "webdav-manager"
           , ProgProj "issuetype" 
           , ProgProj "ticketserver"
           , ProgProj "ticketcli"
           ] 


nameMatch :: [Project] -> String -> Bool
nameMatch projs str = elem str (map projname projs)  

getCabalFileName :: Project -> FilePath 
getCabalFileName (ProgProj pname) = prog </> pname </> (pname ++ ".cabal")
getCabalFileName (WorkspaceProj wname pname) 
  = workspace </> wname </> pname </> (pname ++ ".cabal") 
    
getDependency = map matchDependentPackageName . condTreeConstraints . fromJust . condLibrary

getPkgName = name . pkgName . package . packageDescription
  where name (PackageName str) = str 


combo f g = \x -> (f x , g x)

matchDependentPackageName (Dependency (PackageName x)  _) = x
  

type DaughterMap = M.Map String [String]

convertMotherMapToDaughterMap :: [(String,[String])] -> DaughterMap 
convertMotherMapToDaughterMap = foldl mkDaughterMapWorker M.empty  
  where mkDaughterMapWorker m c = let ps = snd c 
                                  in  foldl (addmeToYourDaughterList c) m ps 
        addmeToYourDaughterList c m p = let f Nothing = Just [fst c]
                                            f (Just cs)  = Just (fst c:cs)    
                                        in  M.alter f p m


dotGraphEach :: (String,[String]) -> String
dotGraphEach (m,ds) = 
  let f x = ("\"" ++ m ++ "\" -> \"" ++ x ++ "\" ;\n") 
  in  concatMap f ds 

dotGraph :: [(String,[String])] -> String
dotGraph lst = "digraph G { \n" ++ concatMap dotGraphEach lst ++ "\n } \n"
  

main = do
  putStrLn "welcome to dev-admin" 
  gdescs <- mapM (readPackageDescription normal . getCabalFileName) projects 
  let deps = map (combo getPkgName getDependency) gdescs
      
      motherlist = map (combo fst (filter (nameMatch projects). snd)) deps
  mapM_ (putStrLn . show ) motherlist 
  putStrLn "daughter map"
  let daughterlist = M.toList ( convertMotherMapToDaughterMap motherlist )
--  mapM_ (putStrLn . show ) )
  putStrLn "-----------------------" 
  writeFile "test.dot" $ dotGraph daughterlist


mai2 = do 
  args <- getArgs
  putStrLn "welcome to dev-admin" 
  gdescs <- mapM (readPackageDescription normal . getCabalFileName) projects 
  let deps = map (combo getPkgName getDependency) gdescs
      
      motherlist = map (combo fst (filter (nameMatch projects). snd)) deps
  mapM_ (putStrLn . show ) motherlist 
  putStrLn "daughter map"
  
  let dmap = convertMotherMapToDaughterMap motherlist
      
  mapM_ cabalInstallJob  $ fromJust .  M.lookup (args !! 0) $ dmap 
  
--  let daughterlist = M.toList ( convertMotherMapToDaughterMap motherlist )
--  mapM_ (putStrLn . show ) )
--  putStrLn "-----------------------" 
--  writeFile "test.dot" $ dotGraph daughterlist


cabalInstallJob name = do 
  putStrLn $ "update : " ++  name
  setCurrentDirectory (prog </> name)
  system $ "cabal install"
