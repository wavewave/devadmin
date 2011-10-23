module Application.DevAdmin.Project where

data Project = WorkspaceProj { workspacename :: String, projname :: String } 
             | ProgProj { projname :: String } 
             deriving (Show,Eq,Ord) 

-- | Part of projects that are availabe in haddock 
--
--   > test
--

partproj :: [Project] 
partproj = [ ProgProj "LHCOAnalysis" 
           , ProgProj "LHCOAnalysis-type" 
           , ProgProj "iteratee-util" 
           , ProgProj "pipeline"
           , ProgProj "madgraph-auto"
           , ProgProj "madgraph-auto-model"
 --           , ProgProj "madgraph-auto-dataset"
 --          , ProgProj "HROOT-generate"
           , ProgProj "HROOT"
           , ProgProj "MSSMType"
           , ProgProj "MSSMScan"
--           , ProgProj "MSSMPlot" 
           , ProgProj "LHEParser"
           , ProgProj "simann"
           , ProgProj "HEPMonteCarlo"
           , ProgProj "ttbar"
           , ProgProj "HEPUtil"
           , ProgProj "iteratee-util"
           , ProgProj "HStringTemplateHelpersIW"
           , ProgProj "webdav-manager"
           , ProgProj "issuetype" 
 --          , ProgProj "ticketserver"
 --          , ProgProj "ticketcli"
           , ProgProj "jobqueue-common"
           , ProgProj "jobqueue-client"
           , ProgProj "jobqueue-server"
           , ProgProj "jobqueue-sender"
           , ProgProj "configparser"
           , ProgProj "devadmin"
           , ProgProj "hmatrixIW"
           , ProgProj "enumerator-util"
  --         , ProgProj "hournal"
           , ProgProj "xournal-parser"
           , ProgProj "xournal-render"
 --          , ProgProj "xournal-web"
           , ProgProj "scaffold"
           , ProgProj "LHE-sanitizer"
           , ProgProj "jobtester"
           , ProgProj "mathematica-data" 
--           , ProgProj "clusteregg" 
           , ProgProj "model-type"
           , ProgProj "model-server"
           , ProgProj "model-client"
           , ProgProj "feynrules-auto"
           ] 

bridgedproj = [ ProgProj "HEPMonteCarlo"
              , ProgProj "HEPUtil"
              , ProgProj "HROOT-generate"
              , ProgProj "HROOT"
              , ProgProj "HStringTemplateHelpersIW"
              , ProgProj "iteratee-util"
              , ProgProj "jobqueue-client"
              , ProgProj "jobqueue-common"
              , ProgProj "jobqueue-sender"
              , ProgProj "jobqueue-server"
              , ProgProj "HStringTemplateHelpersIW"
              , ProgProj "jobtester"
              , ProgProj "LHAPDF-haskell"
              , ProgProj "madgraph-auto-dataset"
              , ProgProj "LHCOAnalysis-type"
              , ProgProj "madgraph-auto-model"
              , ProgProj "LHCOAnalysis"
              , ProgProj "madgraph-auto"
              , ProgProj "LHE-sanitizer"
              , ProgProj "mathematica-data"
              , ProgProj "LHEParser"
              , ProgProj "pipeline"
              , ProgProj "MSSMType"
              , ProgProj "scaffold"
              , ProgProj "configparser"
              , ProgProj "simann"
              , ProgProj "cvmaker" 
              , ProgProj "webdav-manager"
              , ProgProj "devadmin"
              , ProgProj "xournal-parser"
              , ProgProj "enumerator-util"
              , ProgProj "xournal-render"
              , ProgProj "model-type"
              , ProgProj "model-server"
              , ProgProj "model-client"
              , ProgProj "feynrules-auto"
              ]


{-
partproj = [ ProgProj "jobqueue-common"
           , ProgProj "jobqueue-client"
           , ProgProj "jobqueue-server"
           , ProgProj "jobqueue-sender"
           , ProgProj "configparser"
           , ProgProj "pipeline"
           , ProgProj "madgraph-auto"
           , ProgProj "madgraph-auto-model"
           , ProgProj "LHE-sanitizer"
           , ProgProj "LHCOAnalysis"
           , ProgProj "LHCOAnalysis-type"
           , ProgProj "iteratee-util"
           , ProgProj "hmatrixIW"
           ] 
-}


projects :: [Project]
projects = [ ProgProj "LHCOAnalysis" 
           , ProgProj "LHCOAnalysis-type" 
           , ProgProj "iteratee-util" 
           , ProgProj "pipeline"
           , ProgProj "madgraph-auto"
           , ProgProj "madgraph-auto-model"
--           , ProgProj "madgraph-auto-dataset"
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
           , ProgProj "HStringTemplateHelpersIW"
           , ProgProj "webdav-manager"
           , ProgProj "issuetype" 
           , ProgProj "ticketserver"
           , ProgProj "ticketcli"
           , ProgProj "jobqueue-common"
           , ProgProj "jobqueue-client"
           , ProgProj "jobqueue-server"
           , ProgProj "jobqueue-sender"
           , ProgProj "configparser"
           , ProgProj "devadmin"
           , ProgProj "hmatrixIW"
           , ProgProj "enumerator-util"
           , ProgProj "hournal"
           , ProgProj "xournal-parser"
           , ProgProj "xournal-render"
           , ProgProj "xournal-web"
           , ProgProj "scaffold"
           , ProgProj "LHE-sanitizer"
           , ProgProj "jobtester"
           , ProgProj "mathematica-data"
--           , ProgProj "clusteregg" 
           , ProgProj "model-type"
           , ProgProj "model-server"
           , ProgProj "model-client"
           , ProgProj "feynrules-auto"
           , ProgProj "lifemanager"
           ] 

--projects = [ ProgProj "dev-admin" ] 

