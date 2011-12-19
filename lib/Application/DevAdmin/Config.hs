{-# LANGUAGE OverloadedStrings #-}

module Application.DevAdmin.Config where

import Application.DevAdmin.Project
{-
import Text.Parsec 
import HEP.Parser.Config
import Control.Monad.Identity
-}

import Control.Applicative

import Data.Configurator.Types
import Data.Configurator as C
import Control.Monad
import System.Directory 
import System.Environment 
import System.FilePath

data BuildConfiguration = BuildConfiguration { 
  bc_progbase :: FilePath, 
  bc_workspacebase :: FilePath, 
  bc_linkbase :: FilePath, 
  bc_docbase :: FilePath, 
  bc_hoogleDatabase :: FilePath, 
  bc_bridgebase :: FilePath, 
  bc_gitbase :: FilePath, 
  bc_emacsserver :: String
} deriving (Show)

data ProjectConfiguration = ProjectConfiguration { 
  pc_projects :: [Project], 
  pc_bridgeprojects :: [Project], 
  pc_hoogleprojects :: Maybe [Project]
}


loadConfigFile :: IO Config 
loadConfigFile = do 
  homepath <- getEnv "HOME"
  let dotbuild = homepath </> ".build"
  b <- doesFileExist dotbuild 
  when (not b) $ error "no .build file exist at $HOME directory"
  config <- load [Required "$(HOME)/.build"]
  return config

liftM8 :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> r ) 
       -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8
       -> m r
liftM8 f m1 m2 m3 m4 m5 m6 m7 m8 = 
  do { x1 <- m1 ; x2 <- m2 ; x3 <- m3 ; x4 <- m4 ; x5 <- m5 ; x6 <- m6 ; x7 <- m7 ; x8 <- m8 ; return (f x1 x2 x3 x4 x5 x6 x7 x8) }

getBuildConfiguration :: Config -> IO (Maybe BuildConfiguration)
getBuildConfiguration c  = 
  liftM8 BuildConfiguration 
  <$> C.lookup c "build.progbase"
  <*> C.lookup c "build.workspacebase"
  <*> C.lookup c "build.linkbase"
  <*> C.lookup c "build.docbase"
  <*> C.lookup c "build.hoogle"
  <*> C.lookup c "build.bridge"
  <*> C.lookup c "build.git"
  <*> C.lookup c "build.emacsserver"

getProjectConfiguration :: Config -> IO (Maybe ProjectConfiguration)
getProjectConfiguration c  = 
  liftM3 ProjectConfiguration 
  <$> C.lookup c "projects"
  <*> C.lookup c "bridgeproj"
  <*> (C.lookup c "hoogleproj" >>= return . pure ) 

{-

configBuild :: ParsecT String () Identity BuildConfiguration
configBuild = do 
  oneGroupFieldInput "build" $ 
    BuildConfiguration <$> (oneFieldInput "progbase")
                       <*> (oneFieldInput "workspacebase")
                       <*> (oneFieldInput "linkbase")
                       <*> (oneFieldInput "docbase")
                       <*> (oneFieldInput "hoogle")
                       <*> (oneFieldInput "bridge")
                       <*> (oneFieldInput "git")
                       <*> (oneFieldInput "emacsserver")
-}