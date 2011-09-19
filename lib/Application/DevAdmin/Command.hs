{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Command where 

import Application.DevAdmin.Project
import Application.DevAdmin.Graph
import Application.DevAdmin.Config
import Application.DevAdmin.Job
import Application.DevAdmin.ProgType

commandLineProcess :: BuildConfiguration ->  Build -> IO () 
commandLineProcess bc bparam = do 
  alllst <- makeProjDepList bc (map projname projects)
  case bparam of 
    Install {..}     -> do plst <- makeProjDepList bc [pkgname]
                           flip mapM_ plst   (cabalInstallJob bc)
    Push    {..}     ->    flip mapM_ alllst (darcsPushJob bc)
    Haddock {..}     -> do plst <- makeProjDepList bc [pkgname]
                           flip mapM_ plst   (haddockJob bc)
    DepShow {..}     -> do plst <- makeProjDepList bc [pkgname]
                           flip mapM_ plst   (depshowJob bc)
    Pull {..}        ->    flip mapM_ alllst (darcsPullJob bc)
    Hoogle {..}      ->    flip mapM_ alllst (hoogleJob bc)
    Whatsnew {..}    ->    flip mapM_ alllst (darcsWhatsnewJob bc)
    Bootstrap {..}   ->    flip mapM_ alllst (cabalInstallJob bc)
    HaddockBoot {..} ->    flip mapM_ alllst (haddockJob bc)

