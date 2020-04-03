module Day1 where

import Data.List.Split

north :: Dir 
north = 0
east :: Dir
east = 1
south :: Dir
south = 2
west :: Dir
west = 3
initialState :: State
initialState = ((0, 0), 0)

type Move = (Char, Int) 
type Dir = Int
type Pos = (Int, Int)
type State = (Pos, Dir)

parse :: String -> Move
parse (c:cs) = (c, read cs)

parseAll :: String -> [Move]
parseAll s = map parse $ splitOn ", " s

-- Le premier parameter doit etre L ou R
rotate :: Char -> Int -> Int
rotate 'L' i = (i - 1) `mod` 4  
rotate 'R' i = (i + 1) `mod` 4

move :: Dir -> Pos -> Int -> Pos
move 0 (x, y) i = (x, y + i)
move 2 (x, y) i = (x, y - i)
move 1 (x, y) i = (x + i, y)
move 3 (x, y) i = (x - i, y)

update :: State -> Move -> State
update (pos, dir) (c, i) = (pos', d') where
  d'   = rotate c dir 
  pos' = move d' pos i

distance :: (Int, Int) -> Int
distance (p1, p2) = (abs p1) + (abs p2)

main :: IO Int
main = do 
  file <- readFile "day1-data.txt"
  return $ distance $ fst $ foldl update initialState $ parseAll file
