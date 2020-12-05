module Day14_2 where
import Data.List.Split

type Score = Int
type Speed = Int
type Pos = Int
type Reindeer = (Speed, Int, Int)
type Lane = (Reindeer, Pos, [Pos], Score)

r1 :: Reindeer
r1 = (10, 5, 20)
r2 :: Reindeer
r2 = (11, 4, 19)

movesOn :: Int -> Int-> [Pos]
movesOn duration pause = [ x + y*(pause+duration) | y <- [0..], x <- [1..duration] ]

move :: Lane -> Lane
move l = (r, pos + speed, moves, score) where
 (speed, _,  _) = r
 (r, pos, moves, score) = l


advance :: Int -> Lane -> Lane
advance i l  = if i `elem` moves then move l
                                 else l
  where (_, _, moves, _) = l 

advanceAllAndUpdateScore :: [Lane] -> Int -> Int -> [Lane]
advanceAllAndUpdateScore lanes i max = if (i >= max) then lanes
                                                     else advanceAllAndUpdateScore newLanes (i+1) max where
                                                       newLanes = updateScore $ map (advance i) lanes

plusOne :: Lane -> Lane
plusOne (r, p, ps, score) = (r, p, ps, score + 1)

score :: Lane -> Score 
score (_, _, _, s) = s

pos :: Lane -> Pos
pos (_, p, _, _) = p

updateScore :: [Lane] -> [Lane]
updateScore lanes = map f lanes where
  f l = if (max == pos l) then plusOne l
                            else l
  max = maximum $ map pos lanes


parse :: String -> Reindeer
parse s = (read $ w!!3, read $ w!!6, read $ w!!13) where
  w = splitOn " " s


parseAll :: String -> [Reindeer]
parseAll file = map parse $ lines file


lanes :: Reindeer -> Lane
lanes r = (r, 0, takeWhile (<= 2503) $ movesOn dur pause, 0) where 
  (_, dur, pause) = r  


maxScore :: [Lane] -> Int
maxScore lanes = maximum $ map score $ lanes


main :: IO Int
main = do
  lanes <- map lanes <$> parseAll <$> readFile "day14-data.txt"
  return $ maxScore $ advanceAllAndUpdateScore lanes 0 2503 
