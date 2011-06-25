module Config where

import Text.Parsec 
import HEP.Config.Parse
import Control.Monad.Identity
import Control.Applicative

data BuildConfiguration = BuildConfiguration { 
  bc_progbase :: FilePath, 
  bc_workspacebase :: FilePath, 
  bc_linkbase :: FilePath, 
  bc_docbase :: FilePath
} deriving (Show)


configBuild :: ParsecT String () Identity BuildConfiguration
configBuild = do 
  oneGroupFieldInput "build" $ 
    BuildConfiguration <$> (oneFieldInput "progbase")
                       <*> (oneFieldInput "workspacebase")
                       <*> (oneFieldInput "linkbase")
                       <*> (oneFieldInput "docbase")