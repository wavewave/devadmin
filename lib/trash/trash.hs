
{-  
  
--  putStrLn $ show $ gr 
  let str = graphviz gr "test" (1.0,1.0) (1,1) Portrait 
  writeFile "test.dot" str -}

mai1 = do
  putStrLn "welcome to dev-admin" 
  gdescs <- mapM (readPackageDescription normal . getCabalFileName) projects 
  let deps = map (combo getPkgName getDependency) gdescs
      
      motherlist = map (combo fst (filter (nameMatch projects). snd)) deps
  mapM_ (putStrLn . show ) motherlist 
  putStrLn "daughter map"
  let daughterlist = M.toList ( convertMotherMapToDaughterMap motherlist )
--  mapM_ (putStrLn . show ) )
  putStrLn "-----------------------" 
  writeFile "test.dot" $ dotGraph daughterlist


mai2 = do 
  args <- getArgs
  putStrLn "welcome to dev-admin" 
  gdescs <- mapM (readPackageDescription normal . getCabalFileName) projects 
  let deps = map (combo getPkgName getDependency) gdescs
      
      motherlist = map (combo fst (filter (nameMatch projects). snd)) deps
  mapM_ (putStrLn . show ) motherlist 
  putStrLn "daughter map"
  
  let dmap = convertMotherMapToDaughterMap motherlist
      
  mapM_ cabalInstallJob  $ fromJust .  M.lookup (args !! 0) $ dmap 
  
--  let daughterlist = M.toList ( convertMotherMapToDaughterMap motherlist )
--  mapM_ (putStrLn . show ) )
--  putStrLn "-----------------------" 
--  writeFile "test.dot" $ dotGraph daughterlist
