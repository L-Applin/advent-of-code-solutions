import Data.List

findPair :: String -> Bool
findPair []        = False
findPair [a]       = False
findPair [a, b]    = False
findPair [a, b, c] = False
findPair (x:xs)    = if (start `isInfixOf` farTail) then True
                                                    else findPair xs
  where farTail = drop 2 (x:xs)
        start = [x] ++ [head xs]


inBetween :: String -> Bool
inBetween []         = False
inBetween [a]        = False
inBetween [a, b]     = False
inBetween [a, b, c]  = a == c
inBetween (x:xs) = x == head (tail xs) || inBetween xs 
  
type Predicate a = a -> Bool

and :: Predicate a -> Predicate a -> Predicate a
and f g = \a -> (f a) && (g a)

main :: IO Int
main = do
  file <- readFile "day5-data.txt"
  return $ length $ filter (Main.and findPair inBetween) $ lines file 
