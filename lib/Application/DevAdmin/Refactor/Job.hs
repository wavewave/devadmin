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
import System.Directory

import qualified System.IO.Strict as Strict

-- | check file extension and determine whether hs or hsc

checkFileExt :: FilePath -> String -> IO (Maybe String)
checkFileExt srcdir modname = do 
    b1 <- doesFileExist (srcdir </> modname <.> "hs")
    if b1 
      then return (Just "hs")
      else do 
        b2 <- doesFileExist (srcdir </> modname <.> "hsc")
        if b2 
          then return (Just "hsc")
          else return Nothing 


-- | get full file name with correct extension

getFileName :: FilePath -> ModuleName -> IO FilePath 
getFileName srcdir x = do 
    let fp = toFilePath x 
    mext <- checkFileExt srcdir fp 
    case mext of
      Just ext -> return (fp <.> ext)
      Nothing -> error ("no such file "++ fp)


-- | finding import lines and parse it

findImportLines :: FilePath -> IO [Either String ImportLine]
findImportLines fp = do 
  str <- Strict.readFile fp 
  let ls = lines str 
      f str = case parse maybeImportLine "" str of 
                Left err -> error (show err)
                Right Nothing -> Left str
                Right (Just v) -> Right v 
  return (map f ls)


-- | for RefactorTest command

startJobTest :: Refactor -> IO () 
startJobTest param = do 
  putStrLn "startJobTest"
  gdesc <- readPackageDescription normal (pkgPath param </>pkgName param <.> "cabal")
  let Just (CondNode lib y z) = condLibrary gdesc 
      lbi = libBuildInfo lib
      srcdir = head (hsSourceDirs lbi)
  putStrLn $ show lbi 
  filenames <- mapM (getFileName srcdir) (exposedModules lib)
  let g (Left str) = str 
      g (Right il) = importLine2String il 
  forM_ filenames $ \n -> do 
    putStrLn (pkgPath param </> srcdir </> n) 
    ils <- findImportLines (pkgPath param </> srcdir </> n)
    mapM_ (\x -> case x of t@(Right v) -> putStrLn (g t) ; Left e -> return () ) ils 
    -- putStrLn $ unlines (map g ils)
  let modlst = getModules gdesc
  mapM_ putStrLn modlst


-- | for parsetest command 

startParseTest :: String -> IO ()
startParseTest str = do 
  case parse maybeImportLine "" str of 
    Left err -> error (show err)
    Right v -> print v


-- | for renamemodule command 

startRenameModule :: FilePath -> String -> String -> String -> IO () 
startRenameModule pkgpath pkgname modorig modnew = do 
  putStrLn "now starting rename module " 
  gdesc <- readPackageDescription normal (pkgpath </> pkgname <.> "cabal")
  let Just (CondNode lib y z) = condLibrary gdesc 
      lbi = libBuildInfo lib
      srcdir = head (hsSourceDirs lbi)
  putStrLn $ show lbi 
  filenames <- mapM (getFileName srcdir) (exposedModules lib)
  forM_ filenames $ \n -> do 
    let srcpath = pkgpath</>srcdir</>n
    putStrLn srcpath
    ils <- findImportLines srcpath
    writeFile srcpath (replaceModuleNameInImport modorig modnew ils)
   

replaceModuleNameInImport :: String -> String -> [Either String ImportLine] -> String 
replaceModuleNameInImport modorig modnew = unlines . (map worker)   
  where worker (Left str) = str 
        worker (Right il) = if modName il == modorig
                              then importLine2String (il {modName = modnew})
                              else importLine2String il

{-        
  let g (Left str) = return () 
      g (Right il) = do 
        putStr $ "orig = " ++ importLine2String il
        if modName il == modorig
          then putStrLn $ " -> new = " ++ importLine2String (il {modName = modnew})
          else putStrLn "" 
-}