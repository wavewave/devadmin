{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Application.DevAdmin.ProgType
-- Copyright   : (c) 2011 Ian-Woo Kim
-- 
-- License     : BSD3
-- Maintainer  : ianwookim@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Program options for 'build'
--

module Application.DevAdmin.ProgType 
    ( 
    -- * Build Command Type
    -- $build
      Build(..)

    -- * Retrieve Build Command
    , constructBuildModes
    ) where

import System.Console.CmdArgs
import System.Environment 
import System.FilePath

-- $build
--
-- 'build' has the following commands : 'install', 'installseg', 'push', 'haddock', 
-- 'depshow', 'directdepshow', 'pull', 'hoogle', 'whatsnew', 'bootstrap', 'haddockboot'
-- and  'bridge'. 
-- 
--     * install : installing a package and its dependents
--
--     * installseg : 
--
--     * push : 
--
--     * haddock : 
--
--     * depshow : 
--
--     * directdepshow :
--
--     * pull :
--
--     * hoogle : 
--
--     * whatsnew : 
--
--     * bootstrap : 
--
--     * haddockboot : 
--
--     * bridge :  


data Build = Install     { config :: FilePath 
                         , pkgname :: String }
           | InstallSeg  { config :: FilePath 
                         , pkgnamemother :: String 
                         , pkgnamedest :: String } 
           | Push        { config :: FilePath }
           | Haddock     { config :: FilePath 
                         , pkgname :: String } 
           | DepShow     { config :: FilePath
                         , pkgname :: String } 
           | DirectDepShow     { config :: FilePath
                               , pkgname :: String } 
           | Pull        { config :: FilePath }
           | Hoogle      { config :: FilePath } 
           | Whatsnew    { config :: FilePath } 
           | Bootstrap   { config :: FilePath } 
           | HaddockBoot { config :: FilePath }
           | Bridge      { config :: FilePath } 
           | CreateBridge { config :: FilePath 
                          , pkgname :: String } 
             deriving (Show,Data,Typeable)

constructBuildModes :: IO Build 
constructBuildModes = do 
  homedir <- getEnv "HOME"
  let dotbuild = homedir </> ".build"
  let install = Install { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 }
      installseg = InstallSeg { config = dotbuild 
                              , pkgnamemother = "" &= typ "PKGMOTHER" &= argPos 0 
                              , pkgnamedest = "" &= typ "PKGDEST" &= argPos 1 }
      push    = Push    { config = dotbuild }
      haddock = Haddock { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 } 
      depshow = DepShow { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 } 
      directdepshow = DirectDepShow { config = dotbuild 
                                    , pkgname = "" &= typ "PKGNAME" &= argPos 0 } 
      pull    = Pull    { config = dotbuild }
      hoogle  = Hoogle  { config = dotbuild } 
      whatsnew = Whatsnew { config = dotbuild } 
      bootstrap = Bootstrap { config = dotbuild } 
      haddockboot = HaddockBoot { config = dotbuild } 
      bridge = Bridge { config = dotbuild }
      createbridge = CreateBridge { config = dotbuild 
                                  , pkgname = "" &= typ "PKGNAME" &= argPos 0 }

      mode = modes [ install, installseg, push, haddock, directdepshow
                   , depshow, pull, hoogle, whatsnew, bootstrap
                   , haddockboot, bridge, createbridge ] 
              
  return mode 
                        
  

