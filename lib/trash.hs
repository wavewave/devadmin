module ParseConfig where
import Config

{-
configBuild :: ConfigParsec (FilePath,FilePath)
configBuild = do 
  progbase      <- p_dir "progbase"
  workspacebase <- p_dir "workspacebase"
  return (progbase,workspacebase)

p_dir :: String -> ConfigParsec String              
p_dir str = do              
  string str 
  spaces
  char '=' 
  spaces
  val <- many1 (noneOf " \n")
  many (char ' ')
  char '\n'
  return val
-}



