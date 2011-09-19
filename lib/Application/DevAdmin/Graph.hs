module Application.DevAdmin.Graph where

import Data.Maybe
import Data.List 
import Data.Function
import Data.Graph.Inductive
-- import Data.Foldable hiding (concatMap,elem,foldl)

import qualified Data.Map as M

import Application.DevAdmin.Cabal
import Application.DevAdmin.Project
import Application.DevAdmin.Config

import Control.Applicative

import Distribution.Verbosity
import Distribution.PackageDescription.Parse


type DaughterMap = M.Map String [String]

idproj :: [(Int,String)]
idproj = zip [1..] . map projname $ projects

idprojmap :: M.Map Int String 
idprojmap = M.fromList idproj

projidmap :: M.Map String Int 
projidmap = M.fromList $ map swap idproj
  where swap (x,y) = (y,x)

nameMatch :: [Project] -> String -> Bool
nameMatch projs str = elem str (map projname projs)  

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


findAllDaughters :: M.Map String [String] -> String -> [String] 
findAllDaughters m c = do
  case M.lookup c m of
    Just ys -> do y <- ys 
                  c : findAllDaughters m y 
    Nothing -> return c


findOrder :: String -> [String] -> (Int,String) 
findOrder str strs = let (l,_r) = break (== str) strs
                     in  (length l,str)


constructMotherMap ::  BuildConfiguration -> IO (M.Map String [String])
constructMotherMap bc = do 
  let (p,w) = (,) <$> bc_progbase <*> bc_workspacebase $ bc
  gdescs <- mapM (readPackageDescription normal . getCabalFileName (p,w) ) projects
  let deps = map ((,) <$> getPkgName <*> getDependency) gdescs
      mlst = map ((,) <$> fst  <*> (filter (nameMatch projects). snd)) deps
  return (M.fromList mlst)


findAllMothers :: M.Map String [String] -> String -> Maybe [String] 
findAllMothers m proj = 
  case M.lookup proj m of 
    Nothing  -> Nothing  -- [] -- return proj 
    Just lst ->  
      if null lst 
        then return [proj] 
        else do  
          allmothers <- mapM (\x -> findAllMothers m x >>= (\xs -> return (x : xs))) lst  
          return $ nub (concat allmothers)
                        
--   in nub tresult  
-- findAllMothers 


makeProjDepOrderList :: BuildConfiguration -> IO (DaughterMap,[String])
makeProjDepOrderList bc = do 
  putStrLn $ show bc
  let (p,w) = (,) <$> bc_progbase <*> bc_workspacebase $ bc
  gdescs <- mapM (readPackageDescription normal . getCabalFileName (p,w) ) projects 
  let deps = map ((,) <$> getPkgName <*> getDependency) gdescs
      motherlist = map ((,) <$> fst  <*> (filter (nameMatch projects). snd)) deps
      daughtermap = convertMotherMapToDaughterMap motherlist 
      daughterlist = M.toList daughtermap 
      edgelist = concatMap  mkDepEdge daughterlist
      allnodes = idproj 
      gr :: Gr String () 
      gr = mkGraph allnodes edgelist
      linear = topsort gr  
      strlst = map (\x->fromJust $ M.lookup x idprojmap) linear 
  return (daughtermap,strlst)

makeProjDepList :: BuildConfiguration -> [String] -> IO [String]
makeProjDepList bc projs = do 
  (daughtermap,strlst) <- makeProjDepOrderList bc 
     
  let alldaughters = nub . concatMap (findAllDaughters daughtermap) $ projs
      numbered = map (\x -> findOrder x strlst) alldaughters 
      finallist = map snd . sortBy (compare `on` fst) $ numbered 
  return finallist


