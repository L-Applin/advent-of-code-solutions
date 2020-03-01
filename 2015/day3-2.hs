{-
  Advent of Code : Day 3 - 2
  Calculates the number of house visited by Santa and his robot friend
-}
module Day3_2 where  
import Day3

recurMoveRobot :: Pos -> Pos -> [Move] -> Int -> [Pos] -> [Pos]
recurMoveRobot _  _  []     _ pos = pos
recurMoveRobot ps pr (m:ms) i pos = if i `mod` 2 == 0 then recurMoveRobot (nextPos ps) pr ms (i+1) (addPos ps) -- santa moves
                                                      else recurMoveRobot ps (nextPos pr) ms (i+1) (addPos pr) -- robot moves
  where 
    nextPos :: Pos -> Pos
    nextPos p = move p m
    addPos :: Pos -> [Pos]
    addPos p = if (nextPos p `elem` pos) then pos else (pos ++ [nextPos p])

getVisitedRobotPos :: [Move] -> [Pos]
getVisitedRobotPos moves = recurMoveRobot (0, 0) (0,0) moves 0 [(0,0)]

main :: IO Int
main = do
  file <- readFile "day3-data.txt"
  return $ (length . getVisitedRobotPos . parseMoves) file
