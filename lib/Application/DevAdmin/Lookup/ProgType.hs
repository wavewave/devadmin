{-# LANGUAGE DeriveDataTypeable #-}

module Application.DevAdmin.Lookup.ProgType where 

import System.Console.CmdArgs
import System.Environment
import System.FilePath

data Lookup = Lookup { config :: FilePath 
                     , pkgname :: String 
                     , modulename :: String } 
            deriving (Show,Data,Typeable)

constructLookupModes :: IO Lookup 
constructLookupModes = do 
  homedir <- getEnv "HOME"
  let dotbuild = homedir </> ".build"
  let lookup = Lookup { config = dotbuild 
                      , pkgname = "" &= typ "PKGNAME" &= argPos 0
                      , modulename = "" &= typ "MODULENAME" &= argPos 1 
                      }
      mode = modes [ lookup ] 
  return mode
         