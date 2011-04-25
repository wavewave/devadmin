module Graph where

import Data.Maybe
import qualified Data.Map as M
import Project

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
