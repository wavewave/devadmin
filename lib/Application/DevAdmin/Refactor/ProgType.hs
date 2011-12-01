{-# LANGUAGE DeriveDataTypeable #-}

module Application.DevAdmin.Refactor.ProgType where 

import System.Console.CmdArgs

data Refactor = RefactorTest { pkgPath :: String 
                             , pkgName :: String }
              | ParseTest { statement :: String } 
              | RenameModule { pkgPath :: String 
                             , pkgName :: String 
                             , modOrigName :: String
                             , modNewName :: String }
              deriving (Show,Data,Typeable)
        
test = RefactorTest { pkgPath = "" &= typ "PKGPATH" &= argPos 0 
                    , pkgName = "" &= typ "PKGNAME" &= argPos 1 
                    } 
parsetest = ParseTest { statement = "" &= typ "STMT" &= argPos 0
                      }
renamemodule = RenameModule { pkgPath     = "" &= typ "PKGPATH" &= argPos 0
                            , pkgName     = "" &= typ "PKGNAME" &= argPos 1
                            , modOrigName = "" &= typ "MODORIG" &= argPos 2
                            , modNewName  = "" &= typ "MODNEW"  &= argPos 3
                            }


mode :: Refactor 
mode = modes [test, parsetest, renamemodule]
