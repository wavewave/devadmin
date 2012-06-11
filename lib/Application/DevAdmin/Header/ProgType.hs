{-# LANGUAGE DeriveDataTypeable #-}

module Application.DevAdmin.Header.ProgType where 

import System.Console.CmdArgs

data Header = HeaderTest { pkgName :: String }
              deriving (Show,Data,Typeable)

headertest = HeaderTest { pkgName = "" &= typ "PKGPATH" &= argPos 0 
                        } 


mode :: Header 
mode = modes [headertest]
