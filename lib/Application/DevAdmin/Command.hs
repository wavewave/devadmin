{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Command where 

import Application.DevAdmin.Project
import Application.DevAdmin.Graph
import Application.DevAdmin.Config
import Application.DevAdmin.Job
import Application.DevAdmin.ProgType

import Data.List 
import Data.Foldable

import Prelude hiding (mapM_)
-- import Control.Applicative

commandLineProcess :: BuildConfiguration -> ProjectConfiguration -> Build -> IO () 
commandLineProcess bc pc bparam = do 
  let projects = pc_projects pc 
  case bparam of 
    Clone {..} -> do 
      gitCloneJob bc pkgname 
    CloneAll {..}   -> do 
      let projstrs = map projname projects
      forM_ projstrs (gitCloneJob bc)
    _ -> return ()
  alllst <- makeProjDepList bc pc projects 
  case bparam of 
    Install {..}     -> do plst <- makeProjDepList bc pc [ProgProj pkgname] 
                           flip mapM_ plst   (cabalInstallJob bc)
    InstallSeg {..}  -> do plst <- makeProjDepList bc pc (map ProgProj [pkgnamemother]) 
                           -- putStrLn $ show plst 
                           mmap <- (constructMotherMap bc pc)
                           let rallmothers = findAllMothers mmap pkgnamedest
                           case rallmothers of 
                             Nothing -> return () 
                             Just allmothers -> do 
                               let flst = intersect plst (pkgnamedest:allmothers)
                               (putStrLn.show) flst 
                               flip mapM_ flst (cabalInstallJob bc)
    Push    {..}     ->    flip mapM_ alllst (gitPushJob bc)
    Haddock {..}     -> do plst <- makeProjDepList bc pc [ProgProj pkgname]
                           flip mapM_ plst   (haddockJob bc)
    DepShow {..}     -> do plst <- makeProjDepList bc pc [ProgProj pkgname]
                           flip mapM_ plst   (depshowJob bc)
    ShowAllOrdered {..} -> do print alllst 
    DirectDepShow {..} -> do lst <- makeProjDirectDepList bc pc (ProgProj pkgname)
                             putStrLn $ show lst 
    Pull {..}        ->    flip mapM_ alllst (gitPullJob bc)
    -- Hoogle {..}      ->    hoogleJob bc pkgname
    -- HoogleAll {..}   -> do 
    --   let alllst' = case mpkgname of 
    --                   Nothing -> alllst
    --                   Just apkg -> filterBefore apkg alllst 
    --   flip mapM_ alllst' (hoogleJob bc)
    Diff {..}    ->    flip mapM_ alllst (gitDiffJob bc)
    Bootstrap {..}   -> do    
      let alllst' = case mpkgname of 
                      Nothing -> alllst
                      Just apkg -> filterBefore apkg alllst 
      flip mapM_ alllst' (cabalInstallJob bc)
    HaddockBoot {..} -> do 
      let alllst' = case mpkgname of 
                      Nothing -> alllst
                      Just apkg -> filterBefore apkg alllst 
      flip mapM_ alllst' (haddockJob bc)
    -- Bridge {..} -> bridgeJob bc pkgname
    -- BridgeAll {..} -> do 
    --   let balllst = map projname (pc_bridgeprojects pc)
    --   let balllst' = case mpkgname of 
    --                    Nothing -> balllst 
    --                    Just apkg -> filterBefore apkg balllst 
    --   mapM_ (bridgeJob bc) balllst'
    -- CreateBridge {..} ->  createBridgeJob bc pkgname
    CleanAll {..}   -> do    
      let alllst' = case mpkgname of 
                      Nothing -> alllst
                      Just apkg -> filterBefore apkg alllst 
      flip mapM_ alllst' (cabalCleanJob bc)
    HaddockBootSandBox {..} -> do 
      let alllst' = alllst -- case mpkgname of 
                    --   Nothing -> alllst
                    --   Just apkg -> filterBefore apkg alllst 
      flip mapM_ alllst' (haddockSandBoxJob dir bc)


filterBefore name list = dropWhile (/= name) list  