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

-- | 

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
           | ShowAllOrdered { config :: FilePath }
           | Pull        { config :: FilePath }
           | Hoogle      { config :: FilePath 
                         , pkgname :: String }
           | HoogleAll   { config :: FilePath 
                         , mpkgname :: Maybe String 
                         } 
           | Diff        { config :: FilePath } 
           | Bootstrap   { config :: FilePath 
                         , mpkgname :: Maybe String } 
           | HaddockBoot { config :: FilePath 
                         , mpkgname :: Maybe String }
--           | Bridge      { config :: FilePath 
--                         , pkgname :: String }
--           | BridgeAll   { config :: FilePath 
--                         , mpkgname :: Maybe String } 
--           | CreateBridge { config :: FilePath 
--                          , pkgname :: String } 
           | CleanAll { config :: FilePath 
                      , mpkgname :: Maybe String } 
           | CloneAll { config :: FilePath }
           | Clone    { config :: FilePath 
                      , pkgname :: String }
           | HaddockBootSandBox { config :: FilePath
                                , dir :: FilePath } 
           | HaddockSandBox { config :: FilePath
                            , pkgname :: String
                            , dir :: FilePath }  

  --                              , mpkgname :: Maybe String }

             deriving (Show,Data,Typeable)

-- | 

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
      haddocksandbox = HaddockSandBox { config = dotbuild 
                                      , pkgname = "" &= typ "PKGNAME" &= argPos 0  
                                      , dir = "" &= typ "DIRNAME" &= argPos 1
--                                        , mpkgname = def &= typ "RESUMEPKG" &= args  
                                      } 

      depshow = DepShow { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 } 
      directdepshow = DirectDepShow { config = dotbuild 
                                    , pkgname = "" &= typ "PKGNAME" &= argPos 0 }
      showallordered = ShowAllOrdered { config = dotbuild } 
      pull    = Pull    { config = dotbuild }
      hoogle  = Hoogle  { config = dotbuild 
                        , pkgname = "" &= typ "PKGNAME" &= argPos 0 }
      hoogleall  = HoogleAll  { config = dotbuild 
                              , mpkgname = def &= typ "RESUMEPKG" &= args
                              }
      diff  = Diff { config = dotbuild } 
      bootstrap = Bootstrap { config = dotbuild 
                            , mpkgname = def &= typ "RESUMEPKG" &= args
                            } 
      haddockboot = HaddockBoot { config = dotbuild 
                                , mpkgname = def &= typ "RESUMEPKG" &= args
                                } 
--      bridge = Bridge { config = dotbuild 
--                      , pkgname = "" &= typ "PKGNAME" &= argPos 0 }
--      bridgeall = BridgeAll { config = dotbuild 
--                            , mpkgname = def &= typ "RESUMEPKG" &= args
--                            } 
--      createbridge = CreateBridge { config = dotbuild 
--                                  , pkgname = "" &= typ "PKGNAME" &= argPos 0 }
      cleanall = CleanAll { config = dotbuild 
                          , mpkgname = def &= typ "RESUMEPKG" &= args }
      cloneall = CloneAll { config = dotbuild }
      clone  = Clone  { config = dotbuild 
                      , pkgname = "" &= typ "PKGNAME" &= argPos 0 }
      haddockbootsandbox = HaddockBootSandBox { config = dotbuild 
                                              , dir = "" &= typ "DIRNAME" &= argPos 0 
--                                        , mpkgname = def &= typ "RESUMEPKG" &= args  
                                       } 

      mode = modes [ install, installseg, push, haddock, directdepshow
                   , showallordered
                   , depshow, pull, hoogle, hoogleall,  bootstrap
                   , haddockboot, cleanall
                   , cloneall, clone
                   , diff
                   , haddocksandbox
                   , haddockbootsandbox 
                   ] 
--              whatsnew, bridge, bridgeall, createbridge,
  return mode 
                        
