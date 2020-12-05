module JsonParser where
import Parsers
import Control.Applicative
import Data.List.Split

data Json = JsonNull
          | JsonBool Bool
          | JsonNum Int
          | JsonString String
          | JsonArray [Json] 
          | JsonObject [(String, Json)]
          deriving (Show, Eq)


jsonNull :: Parser Json
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser Json
jsonBool = (\s -> JsonBool (s == "true")) <$> (stringP "true" <|> stringP "false") 

jsonInt :: Parser Json
jsonInt = JsonNum <$> intP 

jsonString :: Parser Json
jsonString = JsonString <$> jsonStringLitteral 

jsonArray :: Parser Json
jsonArray = JsonArray <$> (charP '[' *> optWs *> arrayContent <* optWs <* charP ']') where 
  arrayContent = (splitP (optWs *> charP ',' <* optWs) jsonParser)

jsonObject :: Parser Json
jsonObject = JsonObject <$> (charP '{' *> optWs *> splitP (optWs *> charP ',' <* optWs) jsonPair <* optWs <* charP '}') where
      jsonPair = (\key _ value -> (key, value)) <$> jsonStringLitteral <*> (optWs *> charP ':' <* optWs) <*> jsonParser

jsonStringLitteral :: Parser String
jsonStringLitteral = charP '"' *> spanP (\c -> c /= '"') <* charP '"'

jsonParser :: Parser Json
jsonParser = jsonNull 
         <|> jsonBool 
         <|> jsonInt
         <|> jsonString
         <|> jsonArray          
         <|> jsonObject

readJson :: String -> IO (Maybe Json)
readJson s = do
  file <- readFile s
  return $ snd <$> runParser jsonParser file
