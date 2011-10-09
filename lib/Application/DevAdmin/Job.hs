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

depshowJob :: BuildConfiguration -> String -> IO () 
depshowJob _bc name = do 
   putStrLn $ "currently working on " ++ name 



-- | need to be generalized
cabalInstallJob :: BuildConfiguration -> String -> IO () 
cabalInstallJob bc name = do 
  putStrLn $ "update : " ++  name
  system $ "ghc-pkg --force unregister " ++ name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "cabal install"
  return () 


darcsWhatsnewJob :: BuildConfiguration -> String -> IO () 
darcsWhatsnewJob bc name = do 
  putStrLn $ "darcs whatsnew : " ++ name
  setCurrentDirectory ((bc_progbase bc) </> name)
  excode <- system $ "darcs whatsnew"
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
  

darcsPushJob :: BuildConfiguration -> String -> IO () 
darcsPushJob bc name = do 
  putStrLn $ "darcs push : " ++  name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "darcs push"
  return () 

darcsPullJob :: BuildConfiguration -> String -> IO () 
darcsPullJob bc name = do 
  putStrLn $ "darcs pull : " ++  name
  setCurrentDirectory (bc_progbase bc </> name)
  system $ "darcs pull"
  return () 

haddockJob :: BuildConfiguration -> String -> IO () 
haddockJob bc name = do 
  putStrLn $ "haddock : " ++ name 
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "cabal install --enable-documentation"
  system $ "cabal haddock --hyperlink-source"
  system $ "cabal copy"
  versioncheck bc
  return () 

hoogleJob :: BuildConfiguration -> String -> IO () 
hoogleJob bc name = do 
  putStrLn $ "hoogle : " ++ name
  setCurrentDirectory ((bc_progbase bc) </> name) 
  system $ "cabal haddock --hoogle" 
  let hooglefile = (bc_progbase bc) </> name </> "dist/doc/html" </> name </> name ++ ".txt"
  b <- doesFileExist hooglefile 
  if b 
    then copyFile hooglefile  ((bc_hoogleDatabase bc) </> name ++ ".txt")
    else putStrLn $ "no such file : " ++ hooglefile 
  return () 


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



updateHtml :: BuildConfiguration -> ProjectConfiguration -> IO () 
updateHtml bc pc = do
  let projects = pc_projects pc 
  tmpldir <- (</> "template") <$> getDataDir   
  templates <- directoryGroup tmpldir 
  progbodystr <- mapM (progbody bc) projects >>= return . concat

  let str = renderTemplateGroup 
              templates 
              [ ("body" , progbodystr) ] 
              "proghtml.html" 
  writeFile ((bc_linkbase bc) </> "proghtml.html") str

progbody :: BuildConfiguration -> Project -> IO String
progbody bc (ProgProj prjname) = do 
  tmpldir <- (</> "template") <$> getDataDir
  templates <- directoryGroup tmpldir
  let str = renderTemplateGroup
              templates 
              [ ("progindexhtml", "file://" </> (bc_linkbase bc) 
                                            </> prjname 
                                            </> "html/index.html") 
              , ("progname", prjname) ] 
              "progbody.html"
  return str 
progbody _ _ = error "no match error in progbody"
 