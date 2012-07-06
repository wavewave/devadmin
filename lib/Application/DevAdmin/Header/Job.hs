module Application.DevAdmin.Header.Job where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List 

import Text.StringTemplate
import Text.StringTemplate.Helpers 

import Distribution.PackageDescription hiding (author, license)
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.ModuleName

import Application.DevAdmin.Header.ProgType
import Application.DevAdmin.Cabal 
import Application.DevAdmin.Config

import Text.Parsec 
import System.FilePath
import System.Directory

import Application.DevAdmin.Refactor.Job 

import qualified System.IO.Strict as Strict

import Paths_devadmin

data ModuleHeaderInfo = ModuleHeaderInfo { year :: String
                                         , author :: String 
                                         , license :: String 
                                         , email :: String } 


startHeaderJob :: BuildConfiguration -> ProjectConfiguration -> String -> IO () 
startHeaderJob bc pc name = do 
    putStrLn $ "name = " ++ name
    -- putStrLn $ show bc 
    -- putStrLn $ show pc
    let pkgpath =  (bc_srcbase bc </> name)
    setCurrentDirectory pkgpath
    gdesc <- readPackageDescription normal (pkgpath </> name <.> "cabal")
    let Just (CondNode lib y z) = condLibrary gdesc
        lbi = libBuildInfo lib 
        srcdir = head (hsSourceDirs lbi) 
    filenames <- mapM (\x-> (,) <$> getFileName srcdir x <*> (return . intercalate "." . components $ x)) (exposedModules lib)
    putStrLn "year:"
    yearstr <- getLine
    putStrLn "author:"
    authorstr <- getLine 
    putStrLn "license:"
    licensestr <- getLine
    putStrLn "email:"
    emailstr <- getLine  

    let mhinfo = ModuleHeaderInfo yearstr authorstr licensestr emailstr 

    forM_ filenames $ \(n,m) -> do 
      let srcpath = pkgpath </> srcdir </> n
      putStrLn srcpath 
      content <- headerAdjust mhinfo srcpath m 
      print content 


headerAdjust :: ModuleHeaderInfo -> FilePath -> String -> IO ()
headerAdjust mhinfo fp mname = do 
    str <- readFile fp 
    let strs = lines str 
    mapM_ putStrLn . take 10 . map (\(x,y) -> show x ++ ": " ++ y) $ zip [1..] strs 

    tmpldir <- return . (</> "template") =<< getDataDir    
    templates <- directoryGroup tmpldir
    let hstr  = renderTemplateGroup 
                templates 
                [ ( "modulename", mname)
                , ( "year", year mhinfo)
                , ( "author", author mhinfo)
                , ( "license", license mhinfo)
                , ( "email", email mhinfo) ] 
                "moduleheader.hs"
    putStrLn "after which line?"
    lnumstr <- getLine
    let lnum = (read lnumstr :: Int) 
        (bstr,astr) = splitAt lnum strs
    let nstr_final = unlines bstr ++ hstr ++ unlines astr
    putStrLn nstr_final
    writeFile fp nstr_final

    return ()  



    
