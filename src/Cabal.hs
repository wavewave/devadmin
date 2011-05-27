module Cabal where

import System.FilePath
import Data.Maybe
import Project


import Distribution.Package
import Distribution.PackageDescription

getPkgName :: GenericPackageDescription -> String 
getPkgName = name . pkgName . package . packageDescription
  where name (PackageName str) = str 

getDependency :: GenericPackageDescription -> [String]
getDependency = map matchDependentPackageName . condTreeConstraints . fromJust . condLibrary

getCabalFileName :: (FilePath,FilePath) -> Project -> FilePath 
getCabalFileName (prog,_workspace) (ProgProj pname) = prog </> pname </> (pname ++ ".cabal")
getCabalFileName (_prog,workspace) (WorkspaceProj wname pname) 
  = workspace </> wname </> pname </> (pname ++ ".cabal") 

matchDependentPackageName :: Dependency -> String 
matchDependentPackageName (Dependency (PackageName x)  _) = x