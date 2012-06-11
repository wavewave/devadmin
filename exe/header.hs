module Main where

import Application.DevAdmin.Header.ProgType
import Application.DevAdmin.Config
import Application.DevAdmin.Header.Command
import System.Console.CmdArgs 

main :: IO () 
main = do 
  putStrLn "header"
  param <- cmdArgs mode
  print param
  
  commandLineProcess param 
