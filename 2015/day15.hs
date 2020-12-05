module Day15 where
import Data.List.Split
import Text.Read
import Data.Maybe

-- capacity durability flavor texture calories
data Ingredient = Ingredient { capacity :: Int
                             , durability :: Int
                             , flavor :: Int
                             , texture :: Int
                             , calories :: Int
                             } 


parse :: String -> Ingredient 
parse s = Ingredient (value!!2) (value!!4) (value!!6) (value!!8) (value!!10) where
  value = map (fromJust . mRead . filterComma) $ splitOn " " s

filterComma :: String -> String
filterComma s = if last s == ',' then init s else s

mRead :: String -> Maybe Int
mRead = readMaybe


