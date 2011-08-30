module Application.DevAdmin.Job where

import Control.Applicative

import System.Directory
import System.Process
import System.FilePath

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Application.DevAdmin.Config
import Application.DevAdmin.Project
import Application.DevAdmin.VersionCheck
import Paths_devadmin

depshowJob :: BuildConfiguration -> String -> IO () 
depshowJob bc name = do 
   putStrLn $ "currently working on " ++ name 



-- | need to be generalized
cabalInstallJob :: BuildConfiguration -> String -> IO () 
cabalInstallJob bc name = do 
  putStrLn $ "update : " ++  name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "cabal install"
  return () 


darcsWhatsnewJob :: BuildConfiguration -> String -> IO () 
darcsWhatsnewJob bc name = do 
  putStrLn $ "darcs whatsnew : " ++ name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "darcs whatsnew"
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
  copyFile ((bc_progbase bc) </> name </> "dist/doc/html" </> name </> name ++ ".txt") ((bc_hoogleDatabase bc) </> name ++ ".txt")
  return () 



updateHtml :: BuildConfiguration -> IO () 
updateHtml bc = do 
  tmpldir <- (</> "template") <$> getDataDir   
  templates <- directoryGroup tmpldir 
  progbodystr <- mapM (progbody bc) projects >>= return . concat

  let str = renderTemplateGroup 
              templates 
              [ ("body" , progbodystr) ] 
              "proghtml.html" 
  writeFile ((bc_linkbase bc) </> "proghtml.html") str

progbody :: BuildConfiguration -> Project -> IO String
progbody bc (ProgProj projname) = do 
  tmpldir <- (</> "template") <$> getDataDir
  templates <- directoryGroup tmpldir
  let str = renderTemplateGroup
              templates 
              [ ("progindexhtml", "file://" </> (bc_linkbase bc) 
                                            </> projname 
                                            </> "html/index.html") 
              , ("progname", projname) ] 
              "progbody.html"
  return str 

 