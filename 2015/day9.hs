import Data.List
import Data.Maybe
import Data.List.HT
import Data.List.Split
import qualified Data.Map.Strict as Map

type City = String
type Path = ((City, City), Int)

distances :: [Path] -> [City]  -> Int
distances paths cities = sum
                         $ mapAdjacent (\a -> \b -> cityLookup (a, b) paths) cities 

cityLookup :: (City, City) -> [Path] ->  Int
cityLookup (a, b) paths = case (lookup (a, b) paths) of
                            Just i -> i
                            Nothing -> case (lookup (b, a) paths) of
                                         Just j -> j
                                         Nothing -> error "city not found"

parse :: String -> ([City], Path)
parse str = ([c1, c2], ((c1, c2), read d)) where
  words = splitOn " " str
  c1 = words!!0
  c2 = words!!2
  d = words!!4

main :: IO ()
main = do 
  file <- readFile "day9-data.txt"
  let ln = lines file
  let input = map parse ln
  let cities = nub $ foldl (\cs -> \(c, _) -> cs ++ c) [] input
  let paths = map snd input
  let dist = map (distances paths) (permutations cities)
  print $ "max: " ++ (show $ maximum dist) ++ ", min: " ++ (show $ minimum dist)

