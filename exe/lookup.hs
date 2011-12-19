module Main where

import Application.DevAdmin.Lookup.ProgType
import Application.DevAdmin.Config
import Application.DevAdmin.Lookup.Command
import System.Console.CmdArgs 

main :: IO () 
main = do 
  putStrLn "lookup"
  mode <- constructLookupModes
  param <- cmdArgs mode 
  putStrLn $ show param 
  cfg <- loadConfigFile 
  mbc <- getBuildConfiguration cfg 
  case mbc of 
    Nothing -> error ".build file parse error"
    Just bc -> commandLineProcess bc param 
  {-
  configstr <- readFile (config param)
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right bc -> do
      commandLineProcess bc param 
   -}