module Day12 where
import JsonParser

countJson :: Json -> Int -> Int
countJson (JsonNull)  n    = n
countJson (JsonNum i) n    = i + n
countJson (JsonString _) n = n
countJson (JsonArray []) n = n
countJson (JsonArray a)  n = n + (sum $ map f a) where
  f json = countJson json 0
countJson (JsonObject o) n = n + 
  if (JsonString "red" `elem` (map snd o)) then 0 
             else (sum $ map f o) where
               f (key, value) = countJson value 0

main :: IO (Maybe Int)
main = do 
  json <- readJson "day12-data.json"
  return $ fmap (\j -> countJson j 0) json 
