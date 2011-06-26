module Project where

data Project = WorkspaceProj { workspacename :: String, projname :: String } 
             | ProgProj { projname :: String } 
             deriving (Show,Eq,Ord) 

partproj :: [Project] 
partproj = [ ProgProj "jobqueue-common"
           , ProgProj "jobqueue-client"
           , ProgProj "jobqueue-server"
           , ProgProj "jobqueue-sender"
           , ProgProj "configparser"
           , ProgProj "pipeline"
           , ProgProj "madgraph-auto"
           , ProgProj "madgraph-auto-model"
           , ProgProj "madgraph-auto-dataset"
           ] 

projects :: [Project]
projects = [ ProgProj "LHCOAnalysis" 
           , ProgProj "LHCOAnalysis-type" 
           , ProgProj "iteratee-util" 
           , ProgProj "pipeline"
           , ProgProj "madgraph-auto"
           , ProgProj "madgraph-auto-model"
           , ProgProj "madgraph-auto-dataset"
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
           ] 

--projects = [ ProgProj "dev-admin" ] 

