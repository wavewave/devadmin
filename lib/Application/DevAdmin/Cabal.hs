module Application.DevAdmin.Cabal where

import System.FilePath
-- import Data.Maybe
import Application.DevAdmin.Project
import Application.DevAdmin.Config

import Distribution.Package
import Distribution.PackageDescription
import Distribution.ModuleName 
import Distribution.PackageDescription.Parse
import Distribution.Verbosity

import Control.Applicative

import Debug.Trace

getGenPkgDesc :: (FilePath,FilePath) -> Project -> IO GenericPackageDescription 
getGenPkgDesc (progbase,wkspacebase) proj =
  readPackageDescription normal . getCabalFileName (progbase,wkspacebase) $ proj


getAllGenPkgDesc :: BuildConfiguration -> ProjectConfiguration 
                 -> IO [GenericPackageDescription]
getAllGenPkgDesc bc pc = do 
  let projects = pc_projects pc
  let (p,w) = (,) <$> bc_progbase <*> bc_workspacebase $ bc
  mapM (getGenPkgDesc (p,w)) projects


getPkgName :: GenericPackageDescription -> String 
getPkgName = name . pkgName . package . packageDescription
  where name (PackageName str) = str 

getDependency :: GenericPackageDescription -> [String]
getDependency desc = let rlib = condLibrary desc
                     in case rlib of
                          Nothing -> []
                          Just lib -> map matchDependentPackageName . condTreeConstraints $ lib

getCabalFileName :: (FilePath,FilePath) -> Project -> FilePath 
getCabalFileName (prog,_workspace) (ProgProj pname) = prog </> pname </> (pname ++ ".cabal")
getCabalFileName (_prog,workspace) (WorkspaceProj wname pname) 
  = workspace </> wname </> pname </> (pname ++ ".cabal") 

matchDependentPackageName :: Dependency -> String 
matchDependentPackageName (Dependency (PackageName x)  _) = x

{-
getModules :: GenericPackageDescription -> [FilePath]
getModules dsc = 
  let maybelib = library . packageDescription $ dsc 
  in case maybelib of 
       Nothing -> [] 
       Just lib -> map ((<.>"hs") . toFilePath) (exposedModules lib) -}

getModules :: GenericPackageDescription -> [(FilePath,ModuleName)]
getModules dsc = 
  let mnode = condLibrary dsc 
  in maybe [] ((map <$> (,) . head . hsSourceDirs . libBuildInfo <*> exposedModules) . condTreeData) mnode

getOtherModules :: GenericPackageDescription -> [(FilePath,ModuleName)] 
getOtherModules dsc = 
  let mnode = condLibrary dsc 
  in maybe [] ((map <$> (,) . head . hsSourceDirs . libBuildInfo <*> otherModules . libBuildInfo) . condTreeData) mnode

