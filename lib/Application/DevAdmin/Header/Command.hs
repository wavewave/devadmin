{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Header.Command where

import Application.DevAdmin.Header.ProgType 
import Application.DevAdmin.Header.Job
import Application.DevAdmin.Config 

import Control.Applicative

commandLineProcess :: Header -> IO () 
commandLineProcess param@(HeaderTest name) = do 
  putStrLn "header test"
  cfg <- loadConfigFile 
  mbc <- getBuildConfiguration cfg 
  mpc <- getProjectConfiguration cfg 
  maybe (error ".build file parse error")  
        (\(bc,pc)->startHeaderJob bc pc name)
        $ (,) <$> mbc <*> mpc 

