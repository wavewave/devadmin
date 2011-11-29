{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Refactor.Command where

import Control.Applicative

import Application.DevAdmin.Refactor.ProgType 
import Application.DevAdmin.Refactor.Job

commandLineProcess :: Refactor -> IO () 
commandLineProcess param = do 
  putStrLn "refactor test"
  startJobTest param 
{-
  case lparam of 
    Lookup {..} -> do 
      let pkgdir = bc_progbase bc </> pkgname 
      putStrLn $ "pkgdir = " ++  pkgdir
      let (dirname,filename) = moduleDirFile modulename 
      putStrLn $ "dirname = " ++ dirname 
      putStrLn $ "filename = " ++ filename 
      let (p,w) = (,) <$> bc_progbase <*> bc_workspacebase $ bc 
      gdesc <- (readPackageDescription normal . getCabalFileName (p,w) . ProgProj) pkgname
--      putStrLn $ show (condLibrary gdesc)
      case condLibrary gdesc of 
        Nothing -> do putStrLn $ "no library for " ++ pkgname
        Just cnode -> do 
          case condTreeData cnode of 
            Library _ _ lbi -> do 
              let srcdir = head . hsSourceDirs $ lbi 
                  fullpath = pkgdir </> srcdir </> dirname </> filename
              putStrLn fullpath 
              b <- doesFileExist fullpath
              if b 
                then do 
                  system $ "emacsclient --server-file=" ++ (bc_emacsserver bc) ++ " -nw -c --no-wait " ++ fullpath
                  return () 
                else do 
                  putStrLn "No such file exist"
            _ -> do 
              putStrLn "I do not know what to do " 
              putStrLn $ show cnode
 --      putStrLn $ "modulename = " ++ modulename 
   
  -}

