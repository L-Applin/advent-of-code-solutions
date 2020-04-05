module Day2 where


advance :: Int -> Char -> Int 
advance i 'U' = case i of 
                  1 -> 1
                  2 -> 2
                  3 -> 3
                  4 -> 1
                  5 -> 2
                  6 -> 3
                  7 -> 4
                  8 -> 5
                  9 -> 6
advance i 'L' = case i of 
                  1 -> 1
                  2 -> 1
                  3 -> 2
                  4 -> 4
                  5 -> 4
                  6 -> 5
                  7 -> 7
                  8 -> 7
                  9 -> 8
advance i 'D' = case i of
                  1 -> 4
                  2 -> 5
                  3 -> 6
                  4 -> 7
                  5 -> 8
                  6 -> 9
                  7 -> 7
                  8 -> 8
                  9 -> 9
advance i 'R' = case i of 
                  1 -> 2
                  2 -> 3
                  3 -> 3
                  4 -> 5
                  5 -> 6
                  6 -> 6
                  7 -> 8
                  8 -> 9
                  9 -> 9

advanceLine :: Int -> String -> Int
advanceLine i []  = i
advanceLine i [c] = advance i c
advanceLine i s   = foldl advance i s

advanceAccumul :: [Int] -> String -> [Int]
advanceAccumul is s = is ++ [i] where 
  i = advanceLine (last is) s

advanceAll :: [String] -> [Int]
advanceAll strs = tail $ foldl advanceAccumul [5] strs

main :: IO [Int]
main = do
  file <- readFile "day2-data.txt"
  return $ advanceAll $ lines file

