{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Command where 

import Application.DevAdmin.Project
import Application.DevAdmin.Graph
import Application.DevAdmin.Config
import Application.DevAdmin.Job
import Application.DevAdmin.ProgType

import Data.List 

-- import Control.Applicative

commandLineProcess :: BuildConfiguration -> ProjectConfiguration -> Build -> IO () 
commandLineProcess bc pc bparam = do 
  let projects = pc_projects pc 
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
    Push    {..}     ->    flip mapM_ alllst (darcsPushJob bc)
    Haddock {..}     -> do plst <- makeProjDepList bc pc [ProgProj pkgname]
                           flip mapM_ plst   (haddockJob bc)
    DepShow {..}     -> do plst <- makeProjDepList bc pc [ProgProj pkgname]
                           flip mapM_ plst   (depshowJob bc)
    ShowAllOrdered {..} -> do print alllst 
    DirectDepShow {..} -> do lst <- makeProjDirectDepList bc pc (ProgProj pkgname)
                             putStrLn $ show lst 
    Pull {..}        ->    flip mapM_ alllst (darcsPullJob bc)
    Hoogle {..}      ->    hoogleJob bc pkgname
    HoogleAll {..}   -> do 
      let alllst' = case mpkgname of 
                      Nothing -> alllst
                      Just apkg -> filterBefore apkg alllst 
      flip mapM_ alllst' (hoogleJob bc)
    Whatsnew {..}    ->    flip mapM_ alllst (darcsWhatsnewJob bc)
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
    Bridge {..} -> bridgeJob bc pkgname
    BridgeAll {..} -> do 
      let balllst = map projname (pc_bridgeprojects pc)
      let balllst' = case mpkgname of 
                       Nothing -> balllst 
                       Just apkg -> filterBefore apkg balllst 
      mapM_ (bridgeJob bc) balllst'
    CreateBridge {..} ->  createBridgeJob bc pkgname
    CleanAll {..}   -> do    
      let alllst' = case mpkgname of 
                      Nothing -> alllst
                      Just apkg -> filterBefore apkg alllst 
      flip mapM_ alllst' (cabalCleanJob bc)


filterBefore name list = dropWhile (/= name) list  