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

type IDNameMap = M.Map Int String 
type NameIDMap = M.Map String Int

type IDNameAssocList = [(Int,String)]

idproj :: [Project] -> IDNameAssocList
idproj = zip [1..] . map projname 

idprojmap :: [Project] -> IDNameMap
idprojmap = M.fromList . idproj

projidmap :: [Project] -> NameIDMap
projidmap = M.fromList . map swap . idproj
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
  

mkDepEdge :: NameIDMap -> (String, [String]) -> [(Int,Int,())]
mkDepEdge projmap (x,ys) = 
  let idx = fromJust . M.lookup x $ projmap 
  in  map (mkDepEdgeSingle projmap idx) ys
      
mkDepEdgeSingle :: NameIDMap -> Int -> String -> (Int,Int,())       
mkDepEdgeSingle projmap idx y =
  let idy = fromJust . M.lookup y $ projmap 
  in  (idx,idy,())


findAllDaughters :: DaughterMap -> String -> [String] 
findAllDaughters m c = do
  case M.lookup c m of
    Just ys -> do y <- ys 
                  c : findAllDaughters m y 
    Nothing -> return c


findOrder :: String -> [String] -> (Int,String) 
findOrder str strs = let (l,_r) = break (== str) strs
                     in  (length l,str)


constructMotherMap :: BuildConfiguration -> ProjectConfiguration 
                   -> IO (M.Map String [String])
constructMotherMap bc pc = do 
  let projects = pc_projects pc
  gdescs <- getAllGenPkgDesc bc pc 
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


makeProjDepOrderList :: BuildConfiguration 
                     -> ProjectConfiguration
                     -> IO (DaughterMap,[String])
makeProjDepOrderList bc pc = do 
  let projects = pc_projects pc 
  let idnamemap = idprojmap projects
      nameidmap = projidmap projects 
  -- let (p,w) = (,) <$> bc_srcbase <*> bc_workspacebase $ bc
      p = bc_srcbase bc 
  gdescs <- mapM (readPackageDescription normal . getCabalFileName p ) projects 
  let deps = map ((,) <$> getPkgName <*> getDependency) gdescs
      motherlist = map ((,) <$> fst  <*> (filter (nameMatch projects). snd)) deps
      daughtermap = convertMotherMapToDaughterMap motherlist 
      daughterlist = M.toList daughtermap 
      edgelist = concatMap  (mkDepEdge nameidmap) daughterlist
      allnodes = idproj projects
      gr :: Gr String () 
      gr = mkGraph allnodes edgelist
      linear = topsort gr  
      strlst = map (\x->fromJust $ M.lookup x idnamemap) linear 
  return (daughtermap,strlst)

-- | get all dependent daughter packages on a given package

makeProjDepList :: BuildConfiguration -> ProjectConfiguration -> [Project] 
                -> IO [String]
makeProjDepList bc pc projs = do 
  let projnames = map projname projs
  (daughtermap,strlst) <- makeProjDepOrderList bc pc
  let alldaughters = nub . concatMap (findAllDaughters daughtermap) $ projnames
      numbered = map (\x -> findOrder x strlst) alldaughters 
      finallist = map snd . sortBy (compare `on` fst) $ numbered 
  return finallist

-- | get an immediate dependent daughter packages on a given package

makeProjDirectDepList :: BuildConfiguration -> ProjectConfiguration -> Project 
                      -> IO [String]
makeProjDirectDepList bc pc proj = do 
  (daughtermap,_) <- makeProjDepOrderList bc pc
  let r = M.lookup (projname proj) daughtermap 
  case r of 
    Nothing -> return [] 
    Just lst -> return lst



