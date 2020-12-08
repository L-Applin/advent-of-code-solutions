type Seat = (Int, Int)
data Split = Upper | Lower


splitSeat :: Split -> [Int] -> [Int]
splitSeat Upper xs = drop (length xs `div` 2) xs 
splitSeat Lower xs = take (length xs `div` 2) xs 

findSeat :: [Split] -> [Int] -> Int
findSeat _     [a]    = a
findSeat []    (x:xs) = x -- for safety: should never happend :)
findSeat (s:ss) xs    = findSeat ss $ splitSeat s xs

getSplits :: String -> [Split]
getSplits s = map toSplit s where
  toSplit c = case c of
                'F' -> Lower
                'B' -> Upper
                'L' -> Lower
                'R' -> Upper
                _   -> undefined -- means a pb in data, its ok to fail

toSeat :: String -> Seat
toSeat s = (row, col) where
  row = findSeat (getSplits $ take 7 s) [0..127]
  col = findSeat (getSplits $ drop 7 s) [0..7]

seatId :: Seat -> Int
seatId (a,b) = a*8 + b

main :: IO ()
main = do
  allLines <- fmap lines $ readFile "day5-data.txt"
  print $ foldl max 0 $ map (seatId . toSeat) allLines
