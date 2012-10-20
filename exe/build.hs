{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Application.DevAdmin.Config
import Application.DevAdmin.ProgType
import Application.DevAdmin.Command

import Control.Applicative

import Application.DevAdmin.Project
import Data.Text
import Data.Configurator as C

import System.Console.CmdArgs

main :: IO () 
main = do 
  mode <- constructBuildModes 
  param <- cmdArgs mode 
  putStrLn $ show param 
  cfg <- loadConfigFile param 
  mbc <- getBuildConfiguration cfg 
  mpc <- getProjectConfiguration cfg

  -- (test :: Maybe [Project]) <- C.lookup cfg "projects"
  -- putStrLn $ "test = " ++ show test
 
  case (,) <$> mbc <*> mpc of 
    Nothing -> error ".build file parse error"
    Just (bc,pc) -> commandLineProcess bc pc param 
  -- configstr <- readFile (config param)
  -- let conf_result = parse configBuild "" configstr
  -- case conf_result of 
  --   Left err -> putStrLn (show err)
  --  Right bc -> do
  --    commandLineProcess bc param 


