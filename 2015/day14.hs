module Day14 where
import Data.List.Split

type Speed = Int
type Pos = Int
type Reindeer = (Speed, Int, Int)

type Lane = (Reindeer, Pos, [Int])

testR :: Reindeer
testR = (10, 5, 100)

movesOn :: Reindeer -> [Pos]
movesOn (_, duration, pause) = [ x + y*(pause+duration) | y <- [0..], x <- [1..duration] ]

advance :: Lane -> Int -> Lane
advance (r, pos, moves) i = if i `elem` moves then (r, pos + speed, moves)  
                                                          else (r, pos, moves)
                                                             where (speed, duration, pause) = r


advanceFor :: Lane -> Int -> Int -> Int
advanceFor lane i max  = if (i >= max) then pos
                                           else advanceFor r' (i+1) max 
                                             where r' = advance lane i 
                                                   (_, pos, _) = lane

parse :: String -> Reindeer
parse s = (read $ w!!3, read $ w!!6, read $ w!!13) where
  w = splitOn " " s

parseAll :: String -> [Reindeer]
parseAll file = map parse $ lines file

main :: IO Int
main = do
  lanes <- map (\r -> (r, 0, takeWhile (<= 2503) $ movesOn r)) <$> parseAll <$> readFile "day14-data.txt"
  return $ maximum $ map (\l -> advanceFor l 0 2503) lanes
