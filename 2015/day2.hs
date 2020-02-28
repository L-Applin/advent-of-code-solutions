import Data.List.Split
type Box = (Int, Int, Int)

boxArea :: Box -> Int 
boxArea (l, w, h) = (2*l*w) + (2*w*h) + (2*h*l) + smallestSideArea (l, w, h) 

maxSide :: Box -> Int
maxSide (l, w, h) = max l (max w h)

smallestSideArea :: Box -> Int
smallestSideArea (l, w, h) = div (l*w*h) $ maxSide (l, w, h) 

parseLine :: String -> Box
parseLine line = tup3 $ map read $ splitOn "x" line 

tup3 :: [a] -> (a, a, a)
tup3 [x, y, z] = (x, y, z)

areaLine :: String -> Int
areaLine = boxArea . parseLine
 
main :: IO Int
main = do
  file <- readFile "day2-data.txt"
  let sizes = map areaLine (lines file)
  return $ sum sizes


