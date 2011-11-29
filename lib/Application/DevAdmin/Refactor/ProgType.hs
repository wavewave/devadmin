{-# LANGUAGE DeriveDataTypeable #-}

module Application.DevAdmin.Refactor.ProgType where 

import System.Console.CmdArgs

data Refactor = RefactorTest { pkgPath :: String 
                             , pkgName :: String }
              | ParseTest { statement :: String } 
              deriving (Show,Data,Typeable)
        
test = RefactorTest { pkgPath = "" &= typ "PKGPATH" &= argPos 0 
                    , pkgName = "" &= typ "PKGNAME" &= argPos 1 
                    } 
parsetest = ParseTest { statement = "" &= typ "STMT" &= argPos 0
                      }


mode :: Refactor 
mode = modes [test, parsetest]