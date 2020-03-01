{-
  Advent of code : Day 2 - 2
  Meausre the lenght of ribbon required to to wrap all present
-}
module Day2_2 where
import Day2
import Data.List

smallestSidePerimeter :: Box -> Int
smallestSidePerimeter (l, w, h) = (l*w*h) + (x+x+y+y) where
  [x, y, xs] = sort [l, w, h]

main :: IO Int
main = do
  file <- readFile "day2-data.txt"
  let sizes = map (smallestSidePerimeter . parseLine) (lines file)
  return $ sum sizes
  
