{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Refactor.Command where

import Application.DevAdmin.Refactor.ProgType 
import Application.DevAdmin.Refactor.Job

commandLineProcess :: Refactor -> IO () 
commandLineProcess param@(RefactorTest _ _) = do 
  putStrLn "refactor test"
  startJobTest param 
commandLineProcess (ParseTest str) = do 
  putStrLn "parse test"
  startParseTest str


