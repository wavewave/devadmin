module Main where

import System.FilePath 

import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

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

main = do
  putStrLn "welcome to dev-admin" 
  gdescs <- mapM (readPackageDescription normal . getCabalFileName) projects 
  let deps = map (combo getPkgName getDependency) gdescs
      
      motherlist = map (combo fst (filter (nameMatch projects). snd)) deps
  mapM_ (putStrLn . show ) motherlist 

matchDependentPackageName (Dependency (PackageName x)  _) = x
  
  