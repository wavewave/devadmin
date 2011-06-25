module Main where

import System.FilePath 
import System.Environment

import Text.Parsec
import Config
import Project
import Graph
import Cabal
import Job

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




    