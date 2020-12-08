import Data.List

type Seat = (Int, Int)
data Split = Upper | Lower

allSeats :: [Seat]
allSeats = [ (x,y) | x <- [0..127], y <- [0..7] ]

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

getContent :: IO [String]
getContent = fmap lines $ readFile "day5-data.txt"

main :: IO ()
main = do
  allLines <- getContent 
  print $ foldl max 0 $ map (seatId . toSeat) allLines


main_2 :: IO ()
main_2 = do
  allLines <- getContent
  print $ allSeats \\ map toSeat allLines
