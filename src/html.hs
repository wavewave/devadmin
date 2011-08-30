module Main where

import System.FilePath 
import System.Environment

import Text.Parsec
import Application.DevAdmin.Config
import Application.DevAdmin.Project
import Application.DevAdmin.Graph
import Application.DevAdmin.Cabal
import Application.DevAdmin.Job

import Control.Applicative
import Control.Monad


main :: IO ()
main = do   
  putStrLn "html"
  homedir <- getEnv "HOME" 
  configstr <- readFile (homedir </> ".build")
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right bc -> do 
      putStrLn $ show bc
      updateHtml bc




    