module Application.DevAdmin.VersionCheck where

import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import System.FilePath ((</>))
import System.Posix.Files 

import System.Directory

import Application.DevAdmin.Config

isCabal :: String -> Bool 
isCabal str 
  | length str > 6 = let ext = reverse . take 6 . reverse $ str in ext == ".cabal" 
  | otherwise = False
                
cabalName :: Parser String 
cabalName = do 
  manyTill anyChar (try (string "Name:"))
  spaces 
  many1 (noneOf " \n")

cabalVersion :: Parser String
cabalVersion = do
  manyTill anyChar (try (string "Version:"))
  spaces
  many1 (oneOf "0123456789.")
    
nameVersion :: Parser (String,String) 
nameVersion = do 
  n <- cabalName 
  v <- cabalVersion 
  return (n,v)

getProjNameWithVersion :: BuildConfiguration -> String -> IO String 
getProjNameWithVersion bc projname = do 
  let srcbase = bc_srcbase bc 
  cdir <- getCurrentDirectory 
  setCurrentDirectory (srcbase </> projname) 
  currdir <- getDirectoryContents "."
  let cabalfile = head $ filter isCabal currdir
  str <- readFile cabalfile 
  let Right (name,version) = parse nameVersion "" str
      filename = name ++ "-" ++ version
  setCurrentDirectory cdir 
  return filename 

{-  
versioncheck :: BuildConfiguration -> IO ()
versioncheck bc = do 
  putStrLn "version check"
  currdir <- getDirectoryContents "."
  let cabalfile = head $ filter isCabal currdir
  str <- readFile cabalfile 
  let Right (name,version) = parse nameVersion "" str
      filename = name ++ "-" ++ version
      linkpath = bc_linkbase bc </> name 
      origpath = bc_docbase bc </> filename
  
  putStrLn $ "ln -s " ++ origpath ++ " " ++ linkpath
  
  test <- getDirectoryContents (bc_linkbase bc)
  
  if elem name test 
    then do 
      putStrLn "removing link"
      removeLink linkpath  
    else do 
      putStrLn "doesn't exist" 
      return () 
  createSymbolicLink origpath linkpath
    
  return () 
  
-}