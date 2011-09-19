module Main where

import Text.Parsec
import Application.DevAdmin.Config
import Application.DevAdmin.ProgType
import Application.DevAdmin.Command

import System.Console.CmdArgs

main :: IO () 
main = do 
  mode <- constructBuildModes 
  param <- cmdArgs mode 
  putStrLn $ show param 
  configstr <- readFile (config param)
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right bc -> do
      commandLineProcess bc param 


