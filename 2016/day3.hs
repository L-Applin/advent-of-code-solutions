module Day3 where

import Data.List.Split

type Triangle = (Int, Int, Int)

parse :: String -> Triangle
parse s = tup3 $ map read $ wordsBy (==' ') s

parseAll :: String -> (String -> a) -> [a]
parseAll s parser = map parser $ lines s

tup3 :: [a] -> (a, a, a)
tup3 [x, y, z] = (x, y, z)

valid :: Triangle -> Bool
valid (a, b, c) = (a + b > c)
                  &&
                  (a + c > b)
                  &&
                  (b + c > a)

count :: [Triangle] -> Int
count tris = length $ filter valid tris

main :: IO Int
main = do
  file <- readFile "day3-data.txt"
  return $ count $ parseAll file parse
