module Day3_2 where

import Day3
import Data.List.Split

parseCol :: [Int] -> [Triangle]
parseCol nums = [ (nums!!0, nums!!3, nums!!6),
                  (nums!!1, nums!!4, nums!!7),
                  (nums!!2, nums!!5, nums!!8) ]

rearrange :: [Int] -> [Triangle]
rearrange nums =  chunksOf 9 nums >>= parseCol

parseAllCol :: String -> [String]
parseAllCol str = splitOn "\n" str >>= wordsBy (==' ')


main_2 :: IO Int
main_2 = do
  file <- readFile "day3-data.txt"
  return $ count $ rearrange $ map read $ parseAllCol file 

