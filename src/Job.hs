module Job where

import Control.Applicative

import System.Directory
import System.Process
import System.FilePath

import Text.StringTemplate
import Text.StringTemplate.Helpers

import Config
import Project
import VersionCheck
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


darcsPushJob :: BuildConfiguration -> String -> IO () 
darcsPushJob bc name = do 
  putStrLn $ "darcs push : " ++  name
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "darcs push"
  return () 

haddockJob :: BuildConfiguration -> String -> IO () 
haddockJob bc name = do 
  putStrLn $ "haddock : " ++ name 
  setCurrentDirectory ((bc_progbase bc) </> name)
  system $ "cabal install --enable-documentation"
  system $ "cabal haddock --hyperlink-source"
  system $ "cabal install"
  versioncheck bc
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

 