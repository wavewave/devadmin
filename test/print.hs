module Main where

import Application.DevAdmin.Config
import Application.DevAdmin.Refactor.Job

import Control.Monad
import Data.Maybe

import Distribution.PackageDescription 
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.ModuleName hiding (main)

import Application.DevAdmin.Refactor.ProgType
import Application.DevAdmin.Cabal 
import Application.DevAdmin.Refactor.Parse.Import

import Text.Parsec 
import System.FilePath
import System.Directory
import System.Process

import qualified System.IO.Strict as Strict
import Data.List 

makeMangledFileName :: FilePath -> String 
makeMangledFileName fp = intercalate "." (splitDirectories fp)

main :: IO ()
main = do 
  cdir <- getCurrentDirectory 
  let pkgpath = "/Users/iankim/mac/prog/hxournal"
      pkgname = "hxournal"
  putStrLn "print"
  gdesc <- readPackageDescription normal (pkgpath </> pkgname <.> "cabal")
  let Just (CondNode lib y z) = condLibrary gdesc 
      lbi = libBuildInfo lib
      srcdir = head (hsSourceDirs lbi)
  -- putStrLn $ show lbi 
  setCurrentDirectory (pkgpath)
  putStrLn $ srcdir 

  filenames <- mapM (getFileName srcdir) (exposedModules lib)
  setCurrentDirectory (cdir </> "working")

  forM_ filenames $ \n -> do 
    let srcpath = pkgpath</>srcdir</>n
        nfilename = makeMangledFileName n
        command1 = "a2ps --pro=color -M letterdj " ++ srcpath ++ " -o " ++ nfilename <.> "ps"
        command2 = "ps2pdf " ++ (nfilename <.> "ps") 
    system command1 
    system command2 


 -- srcpath
 
