{-# LANGUAGE RecordWildCards #-}

module Application.DevAdmin.Refactor.Parse.Import where

import Text.Parsec 
import Control.Monad.Identity
import Control.Applicative ((<$>),(*>),(<*),(<*>))

import Data.List (intercalate)

data ImportSpec = Import [String] | Hiding [String] | NoSpec
                deriving Show

data ImportQual = NoQual 
                | WeakQual String
                | StrongQual String 
                deriving Show

data ImportLine = ImportLine { modName :: String
                             , qualName :: ImportQual 
                             , importSpec :: ImportSpec
                             }
                deriving Show 

nameLexer :: ParsecT String () Identity String 
nameLexer = many1 (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['.','_']))

nameLexerWithParenSpaces :: ParsecT String () Identity String 
nameLexerWithParenSpaces = do  
    let nonparen = many (noneOf ",(){}")
                     -- (oneOf (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ ['.','_',' ','*','<','>',':','=']))
        paren = do char '(' 
                   str <- nonparen 
                   char ')' 
                   return $ ('(':str)++")"
    (try paren 
     <|> (do s1 <- nonparen 
             s2 <- (try paren
                    <|> return "")  
             return (s1 ++ s2)))
       


importLine :: ParsecT String () Identity ImportLine 
importLine = do spaces 
                string "import"
                skipMany1 space  
                q <- (try (string "qualified" *> return True <* skipMany1 space)
                      <|> return False)
                mname <- nameLexer
                if q then skipMany1 space
                     else spaces
                qname <- if q then (string "as" *> skipMany1 space *> (StrongQual <$> nameLexer))
                              else (try (string "as" *> skipMany1 space *> (WeakQual <$> nameLexer))
                                     <|> return NoQual )
                spaces
                ispec <- impSpec 
                return (ImportLine mname qname ispec)


impSpec :: ParsecT String () Identity ImportSpec
impSpec = try (string "hiding" *> skipMany1 space *> (Hiding <$> tupleP))
          <|> (spaces *> (Import <$> tupleP))
          <|> return NoSpec


tupleP :: ParsecT String () Identity [String] 
tupleP = do char '(' 
            t <- sepBy nameLexerWithParenSpaces (char ',')
            char ')'  
            return t 


maybeImportLine :: ParsecT String () Identity (Maybe ImportLine)
maybeImportLine = (try (Just <$> importLine))
                  <|> 
                  return Nothing 


importLine2String :: ImportLine -> String 
importLine2String ImportLine{..} = 
    case qualName of 
      NoQual -> "import " ++ modName ++ specstr
      WeakQual qname -> "import " ++ modName ++ " as " ++ qname ++ specstr
      StrongQual qname -> "import qualified " ++ modName ++ " as " ++ qname ++ specstr 
  where specstr = case importSpec of 
                    NoSpec -> "" 
                    Import xs -> " (" ++ intercalate "," xs ++ ")"
                    Hiding xs -> " hiding (" ++ intercalate "," xs ++ ")"
