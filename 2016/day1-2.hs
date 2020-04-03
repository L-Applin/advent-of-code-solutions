module Day1_2 where

import Day1
import Data.List

update' :: State -> [Pos] -> [Move] -> Pos
update' (pos, _)   _       []     = pos 
update' (pos, dir) visited (m:ms) = case visited `anyElem` newlyVisited of
                                      Just p  -> p
                                      Nothing -> update' state' (visited ++ newlyVisited) ms
  where 
    newlyVisited = updateVisited pos pos'
    state' = update (pos, dir) m
    (pos', _) = state'
  
anyElem :: Eq a => [a] -> [a] -> Maybe a
anyElem []     _  = Nothing
anyElem [x]    ls = if x `elem` ls then Just x 
                                   else Nothing
anyElem (x:xs) ls = if x `elem` ls then Just x
                                   else anyElem xs ls

updateVisited :: Pos -> Pos -> [Pos]
updateVisited (a, b) (x, y) = if a == x then zip (map (const a) listBY) listBY 
                                        else zip listAX (map (const b) listAX)
                                          where
  listBY = makeList b y
  listAX = makeList a x

makeList :: Int -> Int -> [Int]
makeList a b = if a <= b then [a+1..b]
                         else reverse [b..a-1]


main_2 :: IO Int
main_2 = do
  file <- readFile "day1-data.txt"
  return $ distance $ update' initialState [(0,0)] $ parseAll file 
