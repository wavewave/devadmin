{-# LANGUAGE DeriveDataTypeable #-}

module Application.DevAdmin.ProgType where

import System.Console.CmdArgs
import System.Environment 
import System.FilePath

data Build = Install     { config :: FilePath 
                         , pkgname :: String }
           | Push        { config :: FilePath }
           | Haddock     { config :: FilePath 
                         , pkgname :: String } 
           | DepShow     { config :: FilePath
                         , pkgname :: String } 
           | Pull        { config :: FilePath }
           | Hoogle      { config :: FilePath } 
           | Whatsnew    { config :: FilePath } 
           | Bootstrap   { config :: FilePath } 
           | HaddockBoot { config :: FilePath }
             deriving (Show,Data,Typeable)

constructBuildModes :: IO Build 
constructBuildModes = do 
  homedir <- getEnv "HOME"
  let dotbuild = homedir </> ".build"
  let install = Install { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 }
      push    = Push    { config = dotbuild }
      haddock = Haddock { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 } 
      depshow = DepShow { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 } 
      pull    = Pull    { config = dotbuild }
      hoogle  = Hoogle  { config = dotbuild } 
      whatsnew = Whatsnew { config = dotbuild } 
      bootstrap = Bootstrap { config = dotbuild } 
      haddockboot = HaddockBoot { config = dotbuild } 

      mode = modes [ install, push, haddock, depshow, pull, hoogle
                   , whatsnew, bootstrap, haddockboot ] 
              
  return mode 
                        
  

