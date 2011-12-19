module Main where

import System.FilePath 
import System.Environment

import Application.DevAdmin.Config
import Application.DevAdmin.Job

import Control.Applicative 

main :: IO ()
main = do   
  putStrLn "html"
  -- homedir <- getEnv "HOME" 
  cfg <- loadConfigFile 
  mbc <- getBuildConfiguration cfg 
  mpc <- getProjectConfiguration cfg 
  case (,) <$> mbc <*> mpc of 
    Nothing -> error ".build file parse error"
    Just (bc,pc) -> updateHtml bc pc

  {-
  configstr <- readFile (homedir </> ".build")
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right bc -> do 
      putStrLn $ show bc
      updateHtml bc

  -}


    