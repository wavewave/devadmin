
-----------------------------------------------------------------------------
-- |
-- Module      : Application.DevAdmin.Job 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Application.DevAdmin.Job where

import Control.Applicative

import System.Directory
import System.Process
import System.FilePath
import System.Exit 

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Application.DevAdmin.Config
import Application.DevAdmin.Project
import Application.DevAdmin.VersionCheck
import Paths_devadmin

-- | git clone 

gitCloneJob :: BuildConfiguration -> String -> IO ()
gitCloneJob bc name = do 
  putStrLn $ "git clone : " ++ name
  dir <- getCurrentDirectory
  setCurrentDirectory (dir </> bc_srcbase bc)
  excode <- system $ "git clone " ++ (bc_gitrepobase bc </> name <.> "git")
  case excode of 
    ExitSuccess -> do 
      -- setCurrentDirectory (dir </> bc_srcbase bc </> name)
      -- system $ "git remote add github " ++ (bc_gitrepobase bc </> name <.> "git")
      -- system $ "git push github master"
      return ()
      -- putStrLn "Successful. Press any key." 
      -- c <- getLine
      -- if (not.null $ c)
      --  then return () 
      --  else return ()
    ExitFailure 1 -> do 
      error "Not successful. Press any key." 
      -- c <- getLine
      -- if (not.null $ c)
      --  then return () 
      --  else return ()
    _ -> do 
      error "Not successful. Press any key." 
      -- c <- getLine
      -- if (not.null $ c)
      --  then return () 
      --  else return ()
  setCurrentDirectory dir 
  return () 

-- | git push for a project 

gitPushJob :: BuildConfiguration -> String -> IO () 
gitPushJob bc name = do 
  putStrLn $ "git push : " ++  name
  dir <- getCurrentDirectory 
  setCurrentDirectory (dir </> bc_srcbase bc </> name)
  system $ "git push github master"
  return () 

-- | git pull for a project
gitPullJob :: BuildConfiguration -> String -> IO () 
gitPullJob bc name = do 
  putStrLn $ "git pull : " ++  name
  dir <- getCurrentDirectory 
  setCurrentDirectory (dir </> bc_srcbase bc </> name)
  system $ "git pull github master"
  return () 


-- | 
showJob :: BuildConfiguration -> String -> IO () 
showJob _bc name = do 
   putStrLn $ "currently working on " ++ name 



-- | need to be generalized
cabalInstallJob :: BuildConfiguration -> String -> IO () 
cabalInstallJob bc name = do 
    putStrLn $ "update : " ++  name
    system $ "ghc-pkg --force unregister " ++ name
    dir <- getCurrentDirectory
    setCurrentDirectory (dir </> bc_srcbase bc </> name)
    excode <- system $ "cabal install"
    case excode of 
      ExitSuccess -> do 
        putStrLn "successful installation"
        putStrLn "-----------------------"
      ExitFailure ecd -> error $ "not successful installation of " ++ name
                                  ++ " with exit code " ++ show ecd 
    setCurrentDirectory dir 

  -- return () 


-- | 
haddockJob :: BuildConfiguration -> String -> IO () 
haddockJob bc name = do 
  putStrLn $ "haddock : " ++ name 
  dir <- getCurrentDirectory 
  setCurrentDirectory (dir </> bc_srcbase bc </> name)
  system $ "cabal install --enable-documentation"
  system $ "cabal haddock --hyperlink-source"
  system $ "cabal copy"
  -- versioncheck bc
  return () 


-- | 
cabalCleanJob :: BuildConfiguration -> String -> IO () 
cabalCleanJob bc name = do 
  putStrLn $ "cleaning : " ++  name
  system $ "ghc-pkg --force unregister " ++ name
  dir <- getCurrentDirectory 
  setCurrentDirectory (dir </> bc_srcbase bc </> name)
  excode <- system $ "cabal clean"
  case excode of 
    ExitSuccess -> do 
      putStrLn "successful clean"
      putStrLn "-----------------------"
    ExitFailure ecd -> error $ "not successful installation of " ++ name
                               ++ " with exit code " ++ show ecd 
  -- return () 


-- | 
gitDiffJob :: BuildConfiguration -> String -> IO () 
gitDiffJob bc name = do 
  putStrLn $ "git diff : " ++ name
  dir <- getCurrentDirectory 
  setCurrentDirectory (dir </> bc_srcbase bc </> name)
  excode <- system $ "git diff"
  case excode of 
    ExitSuccess -> do 
      putStrLn "some change happened. would you proceed to the next? (Y/N)" 
      c <- getLine
      if (not.null $ c) &&  (head c == 'y' || head c == 'Y')
        then return () 
        else gitDiffJob bc name 
    ExitFailure 1 -> return () 
    _ -> error $ "do not know what to do in whatsnew job " ++ name 
  return () 


-- | 
haddockSandBoxJob :: FilePath -> BuildConfiguration -> String -> IO () 
haddockSandBoxJob fp bc name = do 
  putStrLn $ "haddock : " ++ name 
  dir <- getCurrentDirectory 
  setCurrentDirectory (dir </> bc_srcbase bc </> name)
  system $ "cabal-dev install --enable-documentation --sandbox="++ fp
  system $ "cabal-dev haddock --hyperlink-source --sandbox=" ++ fp 
  system $ "cabal-dev copy --sandbox=" ++ fp 
  -- versioncheck bc
  return () 

-- | make an index file for each package
updateHtml :: FilePath -> BuildConfiguration -> ProjectConfiguration -> IO () 
updateHtml fp bc pc = do
  let projects = pc_projects pc 
  tmpldir <- (</> "template") <$> getDataDir   
  templates <- directoryGroup tmpldir 
  progbodystr <- mapM (progbody fp bc) projects >>= return . concat
  let str = renderTemplateGroup 
              templates 
              [ ("body" , progbodystr) ] 
              "proghtml.html" 
  writeFile (fp </> "share" </> "doc" </> "index.html") str



progbody :: FilePath -> BuildConfiguration -> Project -> IO String
progbody fp bc (ProgProj prjname) = do 
  prjnameversion <- getProjNameWithVersion bc prjname 
  tmpldir <- (</> "template") <$> getDataDir
  templates <- directoryGroup tmpldir
  let str = renderTemplateGroup
              templates 
              [ ("progindexhtml",  prjnameversion 
                                   </> "html/index.html") 
              , ("progname", prjnameversion) ] 
              "progbody.html"
  return str 
progbody _ _ _ = error "no match error in progbody"



  
{-

-- |
darcsPushJob :: BuildConfiguration -> String -> IO () 
darcsPushJob bc name = do 
  putStrLn $ "darcs push : " ++  name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "darcs push"
  return () 

-- |
darcsPullJob :: BuildConfiguration -> String -> IO () 
darcsPullJob bc name = do 
  putStrLn $ "darcs pull : " ++  name
  setCurrentDirectory (bc_progbase bc </> name)
  system $ "darcs pull"
  return () 

-}


{-
-- | 
hoogleJob :: BuildConfiguration -> String -> IO () 
hoogleJob bc name = do 
  putStrLn $ "hoogle : " ++ name
  setCurrentDirectory ((bc_srcbase bc) </> name) 
  system $ "cabal haddock --hoogle" 
  let hooglefile = (bc_srcbase bc) </> name </> "dist/doc/html" </> name </> name ++ ".txt"
  b <- doesFileExist hooglefile 
  if b 
    then copyFile hooglefile  ((bc_hoogleDatabase bc) </> name ++ ".txt")
    else putStrLn $ "no such file : " ++ hooglefile 
  return () 
-}

-- | 

{-

bridgeJob :: BuildConfiguration -> String -> IO () 
bridgeJob bc name = do 
  putStrLn $ "bridge : " ++ name
  let progdir = bc_progbase bc </> name 
      bridgedir = bc_bridgebase bc </> name ++ "_bridge" 
      bridgedarcs = bridgedir </> name
      bridgegit = bridgedir </> name ++ "_git"
      gitdir = bc_gitbase bc </> name ++ "_git"
  setCurrentDirectory ((bc_progbase bc) </> name) 
  system $ "darcs push " ++ bridgedarcs
  setCurrentDirectory (bc_bridgebase bc) 
  system $ "darcs-fastconvert sync " ++ (name ++ "_bridge") ++ " git"
  setCurrentDirectory gitdir
  system $ "git checkout master"
  system $ "git pull " ++ bridgegit
  system $ "git push github master" 
  return () 

createBridgeJob :: BuildConfiguration -> String -> IO () 
createBridgeJob bc name = do 
  putStrLn $ "create bridge : " ++ name
  let progdir = bc_progbase bc </> name 
      bridgedir = bc_bridgebase bc </> name ++ "_bridge" 
      bridgedarcs = bridgedir </> name
      bridgegit = bridgedir </> name ++ "_git"
      gitdir = bc_gitbase bc </> name ++ "_git"
--  setCurrentDirectory ((bc_progbase bc) </> name) 
--  system $ "darcs push " ++ bridgedarcs
  setCurrentDirectory (bc_bridgebase bc) 
  system $ "darcs-fastconvert create-bridge " ++ progdir 
  setCurrentDirectory (bc_gitbase bc) 
  system $ "git clone " ++ bridgegit 
  setCurrentDirectory bridgegit 
  system $ "git remote add github git@github.com:wavewave/" ++ name  ++ ".git"
  putStrLn $ "please make " ++ name ++ " on github. Did you do?"

  x <- getLine
  if head x == 'y' || head x == 'Y'  
    then system "git push github master " >> return () 
    else putStrLn "later, please do git push github master " 


{-
  system $ "git pull " ++ bridgegit
  system $ "git push github master" -}
  return () 
-}



{-
darcsGetJob :: BuildConfiguration -> String -> IO () 
darcsGetJob bc name = do 
  putStrLn $ "darcs get : " ++ name
  setCurrentDirectory (bc_progbase bc)
  excode <- system $ "darcs get " ++ ((bc_darcsrepobase bc) </> name)
  case excode of 
    ExitSuccess -> do 
      putStrLn "some change happened. would you proceed to the next? (Y/N)" 
      c <- getLine
      if (not.null $ c) &&  (head c == 'y' || head c == 'Y')
        then return () 
        else darcsWhatsnewJob bc name 
    ExitFailure 1 -> return () 
    _ -> error $ "do not know what to do in whatsnew job " ++ name 
  return () 
-}
