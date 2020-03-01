{-
  Advent of code - day 3
  Calculate the number of houses visited by Santa
  https://adventofcode.com/2015/day/3
-}

module Day3 where

type Pos = (Int, Int) -- n/s, e,w
type Dir = Char
north = '^'
east = '>'
west = '<'
south = 'v'

data Move = Move { dir :: Dir
                 , pos :: Pos
                 } deriving (Show, Eq)

northMove = Move north (0, 1)
eastMove =  Move east  (1, 0)
southMove = Move south (0, -1)
westMove =  Move west  (-1, 0)
noMove =    Move 'x'   (0, 0)

addPos :: Pos -> Pos -> Pos 
addPos (a, b) (c, d) = (a+c, b+d)

move :: Pos -> Move -> Pos
move p m = addPos p (pos m)

recurMove :: Pos -> [Move] -> [Pos] -> [Pos]
recurMove p []     ps = ps
recurMove p (m:ms) ps = if (nextPos `elem` ps) then recurMove nextPos ms ps
                                               else recurMove nextPos ms (ps ++ [nextPos]) 
  where nextPos = move p m

getVisitedPos :: [Move] -> [Pos]
getVisitedPos moves = recurMove (0, 0) moves [(0,0)] 

parseMoves :: String -> [Move]
parseMoves str = map toMove str where
  toMove c = case c of
               '^' -> northMove
               'v' -> southMove
               '>' -> eastMove
               '<' -> westMove
               _   -> noMove

main :: IO Int
main = do
  file <- readFile "day3-data.txt"
  return $ (length . getVisitedPos . parseMoves) file
