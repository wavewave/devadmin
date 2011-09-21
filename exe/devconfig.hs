{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char
import Data.List
import Application.DevAdmin.Project
import System.Console.CmdArgs

data Devconfig = Hoogle | Other 
  deriving (Show,Data,Typeable) 

mode :: Devconfig
mode = modes [Hoogle, Other]

commandProcess :: Devconfig -> IO ()
commandProcess Hoogle = do 
  let strs = intercalate " " (map f partproj)
  putStr strs 
 where f x = case x of 
               ProgProj str -> '+' : map toLower str
               _ -> undefined   
commandProcess Other = do 
  putStrLn "" 


main :: IO ()
main = do 
  param <- cmdArgs mode
  commandProcess param