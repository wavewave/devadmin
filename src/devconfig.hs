{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.List
import Project
import System.Console.CmdArgs

data Devconfig = Hoogle | Other 
  deriving (Show,Data,Typeable) 

mode = modes [Hoogle, Other]

commandProcess Hoogle = do 
  let strs = intercalate " " (map f partproj)
  putStr strs 
 
 where f x = case x of 
               ProgProj str -> '+' : str  


main = do 
  param <- cmdArgs mode
  commandProcess param