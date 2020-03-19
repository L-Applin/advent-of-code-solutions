module Day13 where
import Data.List
import Data.Maybe
import Data.List.HT
import Data.List.Split
import qualified Data.Map.Strict as Map

type Person = String
type Happiness = Int
type Seating = ((Person, Person), Happiness)
  
self :: String
self = "__self__"

totalHapiness :: [Seating] -> [Person] -> Int
totalHapiness seatings persons = (sum $ mapAdjacent (\a b -> personLookup (a, b) seatings) persons)
                                 +
                                 (personLookup (head persons, last persons) seatings) -- it's circular => last and first person

personLookup :: (Person, Person) -> [Seating] -> Int
personLookup (a, b) seatings = if (a == self || b == self) 
                                  then 0 
                                  else  fromJust $ (+) <$> (lookup (a, b) seatings) <*>  (lookup (b, a) seatings)

parse :: String -> ([Person], Seating)
parse str = ([p1, p2], ((p1, p2), d)) where
  words = splitOn " " str
  p1 = words!!0
  p2 = init $ words!!10
  d = case (words!!2) of 
        "gain" -> read $ words!!3 
        "lose" -> 0 - (read $ words!!3)

main :: IO Int
main = do
  file <- readFile "day13-data.txt"
  let input = map parse $ lines file 
  let persons = self:(nub $ foldl (\cs (c, _) -> cs ++ c) [] input)
  let seatings = map snd input
  let hapiness = map (totalHapiness seatings) (permutations persons) 
  return $ maximum hapiness
