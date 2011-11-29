{-# LANGUAGE DeriveDataTypeable #-}

module Application.DevAdmin.Refactor.ProgType where 

import System.Console.CmdArgs

data Refactor = RefactorTest { pkgPath :: String 
                             , pkgName :: String }
                   {- Refactor { config :: FilePath 
                         , pkgname :: String 
                         , modulename :: String }  -}
                deriving (Show,Data,Typeable)
        
test = RefactorTest { pkgPath = "" &= typ "PKGPATH" &= argPos 0 
                    , pkgName = "" &= typ "PKGNAME" &= argPos 1 } 

mode :: Refactor 
mode = modes [test]