module Main where

import System.FilePath 
import System.Environment

import Application.DevAdmin.Config
import Application.DevAdmin.Job

main :: IO ()
main = do   
  putStrLn "html"
  -- homedir <- getEnv "HOME" 
  cfg <- loadConfigFile 
  mbc <- getBuildConfiguration cfg 
  case mbc of 
    Nothing -> error ".build file parse error"
    Just bc -> updateHtml bc

  {-
  configstr <- readFile (homedir </> ".build")
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right bc -> do 
      putStrLn $ show bc
      updateHtml bc

  -}


    