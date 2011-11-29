module Application.DevAdmin.Refactor.Job where

import Control.Monad
import Data.Maybe

import Distribution.PackageDescription 
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.ModuleName

import Application.DevAdmin.Refactor.ProgType
import Application.DevAdmin.Cabal 
import Application.DevAdmin.Refactor.Parse.Import

import Text.Parsec 
import System.FilePath

startJobTest :: Refactor -> IO () 
startJobTest param = do 
  putStrLn "startJobTest"
  gdesc <- readPackageDescription normal (pkgPath param </>pkgName param <.> "cabal")
  let Just (CondNode lib y z) = condLibrary gdesc 
      lbi = libBuildInfo lib
      srcdir = head (hsSourceDirs lbi)
  putStrLn $ show lbi 
  let filenames = map ((<.>"hs") . toFilePath) (exposedModules lib)
      --g (Left str) = str 
      --g (Right il) = importLine2String il 
  forM_ filenames $ \n -> do 
    putStrLn (pkgPath param </> srcdir </> n) 
    ils <- findImportLines (pkgPath param </> srcdir </> n)
    mapM_ (\x -> case x of Right v -> print v ; Left e -> return () ) ils 
    -- putStrLn $ unlines (map g ils)
  let modlst = getModules gdesc
  mapM_ putStrLn modlst


findImportLines :: FilePath -> IO [Either String ImportLine]
findImportLines fp = do 
  str <- readFile fp 
  let ls = lines str 
      f str = case parse maybeImportLine "" str of 
                Left err -> error (show err)
                Right Nothing -> Left str
                Right (Just v) -> Right v 
  return (map f ls)


startParseTest :: String -> IO ()
startParseTest str = do 
  case parse maybeImportLine "" str of 
    Left err -> error (show err)
    Right v -> print v