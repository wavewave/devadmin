module Main where

import Text.Parsec
import Application.DevAdmin.Refactor.ProgType
import Application.DevAdmin.Config
import Application.DevAdmin.Refactor.Command
import System.Console.CmdArgs 

main :: IO () 
main = do 
  putStrLn "refactor"
  param <- cmdArgs mode
  print param
  
  commandLineProcess param 
