{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative 
import Control.Monad

import System.Directory 
import System.FilePath
import System.Locale
import System.Process
import Data.Word
import Data.Time

import Application.DevAdmin.Config
import Application.DevAdmin.Project

-- import HROOT

main :: IO ()
main = do 
  putStrLn "show all darcs patches"

  cfg <- loadConfigFile
  mbc <- getBuildConfiguration cfg 
  mpc <- getProjectConfiguration cfg 
  case (mbc,mpc) of 
    (Just bc, Just pc) -> do 
      cdir <- getCurrentDirectory
      lst <- return . concat =<< mapM (getCommitDays bc) (pc_projects pc)
      -- putStrLn . show $ lst 
      print $ length lst
      setCurrentDirectory cdir
      plot lst
    _ -> do 
      putStrLn "parse error in config file"




getCommitDays :: BuildConfiguration -> Project -> IO [Day]
getCommitDays bc proj = do
  setCurrentDirectory (bc_progbase bc </> projname proj) 
  result <- readProcess "darcs" ["changes"] ""  
  let r2 = map (take 30) 
           . filter (\x->head x /= ' ') 
           . filter (not.null) 
           . lines 
           $ result
      parseddays :: [Day] = map (readTime defaultTimeLocale "%a %b %e %H:%M:%S %Z %Y ") r2
  return parseddays


plot :: [Day] -> IO ()  
plot vs = do 
  tcanvas <- newTCanvas "test" "test" 640 480
  cd tcanvas 0

  t0 <- newTDatime 2010 01 01 0 0 0 
  x0 <- convert t0 0
  t1 <- newTDatime 2010 01 01 00 00 00 
  x1 <- (-) <$> convert t1 0 <*> return x0 
  t2 <- newTDatime 2012 03 01 00 00 00  
  x2 <- (-) <$> convert t2 0 <*> return x0 

  putStrLn $ show x0 
  putStrLn $ show x1
  putStrLn $ show x2 

  let ndays = diffDays (fromGregorian 2012 03 01) (fromGregorian 2010 01 01)
      nbins = fromIntegral ndays `div` 30  -- `div` 7 

  h1 <- newTH1F "test" "test"  nbins (fromIntegral x1) (fromIntegral x2)

  xaxis <- tH1GetXaxis (upcastTH1 h1) 
  setTimeOffset xaxis (fromIntegral x0) "local"
  setTimeDisplay xaxis 1
  setTimeFormat xaxis "%Y/%m/%d"
  setTimeDisplay xaxis 33

  setLabelOffset xaxis 99
  let lst = [1, 1+(nbins`div`20).. nbins ]
  print lst 
  draw h1 ""   
  t <- newTText 100 100 "test"
  tmp <- newTDatime 2010 01 01 0 0 0

  setTextAlign t 33
  setTextAngle t 60 
  setTextSize t 0.02
 
  forM_ lst $ \item -> do 
     x <- getBinCenterTAxis xaxis item
     putStrLn (show x) 
     setTDatime tmp (floor x + x0) 
     (yy,mm,dd) <- (,,) <$> tDatimeGetYear tmp 
                        <*> tDatimeGetMonth tmp 
                        <*> tDatimeGetDay tmp 
     drawText t x (-1) (show yy ++ "/" ++ show mm ++ "/" ++ show dd)
    
  mapM_ (fillDay x0 h1) vs
  saveAs tcanvas "datetime.pdf" "" 
  delete h1
  delete tcanvas

  return () 

fillDay :: Word -> TH1F -> Day -> IO () 
fillDay offset h1 day = do 
  let (yyyy,mm,dd) = toGregorian day 
  t1 <- newTDatime (fromIntegral yyyy) mm dd 12 00 00 
  x1 <- (-) <$> convert t1 0 <*> return offset 

  let val = fromIntegral x1 
  fill1 h1 val 
  return ()
{-
  let go n | n > 0 = do fill1 h1 val 
                        go (n-1)
           | otherwise = return () 
  go num 
-}