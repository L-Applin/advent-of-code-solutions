import Data.List

pairwise :: [a] -> [(a, a)]
pairwise l = [ (x,y) | (x:ys) <- tails l, y <- ys ]

isSum :: Num a => Eq a => a -> (a, a) -> Bool
isSum i (a ,b) = i == (a + b) 

twoEntriesThatSumUpTo :: Num a => Eq a => a -> [a] -> (a, a)
twoEntriesThatSumUpTo target xs = head $ filter (isSum target) $ pairwise xs

findProduct :: Num a => Eq a => a -> [a] -> a
findProduct i xs = a * b where
  (a, b) = twoEntriesThatSumUpTo i xs

main :: IO Int 
main = do
  content <- readFile "input-day1.txt"
  let allLines = map read $ lines content 
  return $ findProduct 2020 allLines
