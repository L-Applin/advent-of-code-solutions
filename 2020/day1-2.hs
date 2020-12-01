import Data.List

triplewise :: Num a => Eq a => a -> [a] -> [(a, a, a)]
triplewise i l = [ (x,y,z) 
  | (x:ys) <- tails l
  , (y:zs) <- tails ys
  , z <- zs
  , x+y+z == i ]

findProduct :: Num a => Eq a => a -> [a] -> a
findProduct i xs = a * b * c where
  (a, b, c) = head $ triplewise i xs 

main :: IO Int 
main = do
  content <- readFile "input.txt"
  let allLines = map read $ lines content 
  return $ findProduct 2020 allLines
