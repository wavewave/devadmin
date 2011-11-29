{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Refactor.Parse.Import where

import Text.Parsec 
import Control.Monad.Identity
import Control.Applicative ((<$>))

data ImportLine = ImportLine { modName :: String
                             , qualifiedName :: Maybe String 
                             , importSymbols :: [String]
                             , hidingSymbols :: [String]
                             , restOfLine :: String 
                             }
                deriving Show 

importLine :: ParsecT String () Identity ImportLine 
importLine = do spaces 
                string "import"
                skipMany1 space  
                str <- many1 (noneOf "\n ")
                spaces 
                rest <- many (noneOf "\n")
                return (ImportLine str Nothing [] [] rest)

maybeImportLine :: ParsecT String () Identity (Maybe ImportLine)
maybeImportLine = (try (Just <$> importLine))
                  <|> 
                  return Nothing 

{-
hiding :: ParsecT String () Identity [String] 
hiding = do 
  string "hiding" 
-}  


importLine2String :: ImportLine -> String 
importLine2String ImportLine{..} = "import " ++ modName ++ " " ++ restOfLine