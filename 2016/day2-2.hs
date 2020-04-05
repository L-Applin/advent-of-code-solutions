module Day2_2 where

advance' :: Char -> Char -> Char 
advance' c 'U' = case c of
                   '1' -> '1'
                   '2' -> '2'
                   '3' -> '1'
                   '4' -> '4'
                   '5' -> '5'
                   '6' -> '2'
                   '7' -> '3'
                   '8' -> '4'
                   '9' -> '9'
                   'A' -> '6'
                   'B' -> '7'
                   'C' -> '8'
                   'D' -> 'B'
advance' c 'R' = case c of
                   '1' -> '1'
                   '2' -> '3'
                   '3' -> '4'
                   '4' -> '4' 
                   '5' -> '6'
                   '6' -> '7'
                   '7' -> '8'
                   '8' -> '9'
                   '9' -> '9'
                   'A' -> 'B'
                   'B' -> 'C'
                   'C' -> 'C'
                   'D' -> 'D'
advance' c 'D' = case c of
                   '1' -> '3'
                   '2' -> '6'
                   '3' -> '7'
                   '4' -> '8' 
                   '5' -> '5'
                   '6' -> 'A'
                   '7' -> 'B'
                   '8' -> 'C'
                   '9' -> '9'
                   'A' -> 'A'
                   'B' -> 'D'
                   'C' -> 'C'
                   'D' -> 'D'
advance' c 'L' = case c of
                   '1' -> '1'
                   '2' -> '2'
                   '3' -> '2'
                   '4' -> '3' 
                   '5' -> '5'
                   '6' -> '5'
                   '7' -> '6'
                   '8' -> '7'
                   '9' -> '8'
                   'A' -> 'A'
                   'B' -> 'A'
                   'C' -> 'B'
                   'D' -> 'D'

advanceLine :: Char -> String -> Char
advanceLine i []  = i
advanceLine i [c] = advance' i c
advanceLine i s   = foldl advance' i s
 
advanceAccumul :: [Char] -> String -> [Char]
advanceAccumul is s = is ++ [i] where
     i = advanceLine (last is) s
  
advanceAll :: [String] -> [Char]
advanceAll strs = tail $ foldl advanceAccumul ['5'] strs

main :: IO [Char]
main = do
  file <- readFile "day2-data.txt"
  return $ advanceAll $ lines file
