{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char
import Data.List
import Data.Maybe
import Application.DevAdmin.Project
import Application.DevAdmin.Config
import System.Console.CmdArgs

data Devconfig = Hoogle | Other 
  deriving (Show,Data,Typeable) 

mode :: Devconfig
mode = modes [Hoogle, Other]

commandProcess :: Devconfig -> ProjectConfiguration -> IO ()
commandProcess Hoogle pc = do 
  let mhoogleproj = pc_hoogleprojects pc
  maybe (return ()) `flip` mhoogleproj $ \hoogleproj -> do 
    let strs = intercalate " " (map f hoogleproj)
    putStr strs 
   where f x = case x of 
                 ProgProj str -> '+' : map toLower str
                 _ -> undefined   
commandProcess Other _ = do 
  putStrLn "" 


main :: IO ()
main = do 
  param <- cmdArgs mode
  cfg <- loadConfigFile 
  mpc <- getProjectConfiguration cfg 
  case mpc of 
    Nothing -> error ".build file parse error"
    Just pc -> commandProcess param pc