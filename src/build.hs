module Main where

import System.FilePath 
import System.IO
import System.Environment
import System.Process
import System.Directory 

import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

import qualified Data.Map as M

import Data.Maybe
import Data.List 
import Data.Function

import Data.Graph.Inductive 
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Graphviz 

import Text.Parsec
import ParseConfig

data Project = WorkspaceProj { workspacename :: String, projname :: String } 
             | ProgProj { projname :: String } 
             deriving (Show,Eq,Ord) 

projects = [ ProgProj "LHCOAnalysis" 
           , ProgProj "LHCOAnalysis-type" 
           , ProgProj "iteratee-util" 
           , ProgProj "pipeline"
           , ProgProj "madgraph-auto"
           , ProgProj "madgraph-auto-model"
           , ProgProj "madgraph-auto-dataset"
           , ProgProj "HROOT-generate"
           , ProgProj "HROOT"
           , ProgProj "MSSMType"
           , ProgProj "MSSMScan"
           , ProgProj "MSSMPlot" 
           , ProgProj "LHEParser"
           , ProgProj "simann"
           , ProgProj "HEPMonteCarlo"
           , ProgProj "ttbar"
           , ProgProj "HEPUtil"
           , ProgProj "iteratee-util"
--           , ProgProj "HStringTemplateHelpersIW"
           , ProgProj "webdav-manager"
           , ProgProj "issuetype" 
           , ProgProj "ticketserver"
           , ProgProj "ticketcli"
--           , ProgProj "dev-admin"
           ] 

--projects = [ ProgProj "dev-admin" ] 

idproj :: [(Int,String)]
idproj = zip [1..] . map projname $ projects

idprojmap = M.fromList idproj

projidmap = M.fromList $ map swap idproj
  where swap (x,y) = (y,x)

nameMatch :: [Project] -> String -> Bool
nameMatch projs str = elem str (map projname projs)  

getCabalFileName :: (FilePath,FilePath) -> Project -> FilePath 
getCabalFileName (prog,workspace) (ProgProj pname) = prog </> pname </> (pname ++ ".cabal")
getCabalFileName (prog,workspace) (WorkspaceProj wname pname) 
  = workspace </> wname </> pname </> (pname ++ ".cabal") 

getDependency = map matchDependentPackageName . condTreeConstraints . fromJust . condLibrary

getPkgName = name . pkgName . package . packageDescription
  where name (PackageName str) = str 


combo f g = \x -> (f x , g x)

matchDependentPackageName (Dependency (PackageName x)  _) = x
  

type DaughterMap = M.Map String [String]

convertMotherMapToDaughterMap :: [(String,[String])] -> DaughterMap 
convertMotherMapToDaughterMap = foldl mkDaughterMapWorker M.empty  
  where mkDaughterMapWorker m c = let ps = snd c 
                                  in  foldl (addmeToYourDaughterList c) m ps 
        addmeToYourDaughterList c m p = let f Nothing = Just [fst c]
                                            f (Just cs)  = Just (fst c:cs)    
                                        in  M.alter f p m


dotGraphEach :: (String,[String]) -> String
dotGraphEach (m,ds) = 
  let f x = ("\"" ++ m ++ "\" -> \"" ++ x ++ "\" ;\n") 
  in  concatMap f ds 

dotGraph :: [(String,[String])] -> String
dotGraph lst = "digraph G { \n" ++ concatMap dotGraphEach lst ++ "\n } \n"
  

mkDepEdge :: (String, [String]) -> [(Int,Int,())]
mkDepEdge (x,ys) = 
  let idx = fromJust . M.lookup x $ projidmap 
  in  map (mkDepEdgeSingle idx) ys
      
mkDepEdgeSingle :: Int -> String -> (Int,Int,())       
mkDepEdgeSingle idx y =
  let idy = fromJust . M.lookup y $ projidmap 
  in  (idx,idy,())


-- | need to be generalized
cabalInstallJob :: FilePath -> String -> IO () 
cabalInstallJob prog name = do 
  putStrLn $ "update : " ++  name
  setCurrentDirectory (prog </> name)
  system $ "cabal install"
  return () 

main :: IO ()
main = do   
  args <- getArgs 
  homedir <- getEnv "HOME"
  putStrLn $ "build " ++ (args !! 0)
  putStrLn $ "reading " ++ (homedir </> ".build")
  configstr <- readFile (homedir </> ".build")
  let conf_result = parse configBuild "" configstr
  case conf_result of 
    Left err -> putStrLn (show err)
    Right (p,w) -> do 
      gdescs <- mapM (readPackageDescription normal . getCabalFileName (p,w) ) projects 
      let deps = map (combo getPkgName getDependency) gdescs
          motherlist = map (combo fst (filter (nameMatch projects). snd)) deps
          daughtermap = convertMotherMapToDaughterMap motherlist 
          daughterlist = M.toList daughtermap 
          edgelist = concatMap  mkDepEdge daughterlist
          allnodes = idproj 
          gr :: Gr String () 
          gr = mkGraph allnodes edgelist
          linear = topsort gr  
          strlst = map (\x->fromJust $ M.lookup x idprojmap) linear 
     
      let alldaughters = nub $ findAllDaughters daughtermap  (args !! 0)
          numbered = map (\x -> findOrder x strlst) alldaughters 
          finallist = map snd . sortBy (compare `on` fst) $ numbered 

      mapM_ (cabalInstallJob p) finallist 

--      putStrLn $ show finallist
--      putStrLn (show daughterlist ) 
--      putStrLn . show $ nub $   
--       mapM_ (cabalInstallJob p)  r 

findAllDaughters :: M.Map String [String] -> String -> [String] 
findAllDaughters m c = do
  case M.lookup c m of
    Just ys -> do y <- ys 
                  c : findAllDaughters m y 
    Nothing -> return c


findOrder :: String -> [String] -> (Int,String) 
findOrder str strs = let (l,r) = break (== str) strs
                     in  (length l,str)

    