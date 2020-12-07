tree :: Char
tree = '#'

type Area = [String]
type Slope = (Int, Int)

slopes :: [Slope]
slopes  = [(1,1)
          ,(3,1)
          ,(5,1)
          ,(7,1)
          ,(1,2)
          ]

move :: Int -> Int -> String -> Area -> Int 
move i _ _      []     = i
move i n (c:cs) (x:xs) = move (incrementIfIsTree i c) (n+3) next xs where
  next = drop (n+3) $ head xs
               
moveWithSlope :: Int -> Int -> Area -> Slope -> Int
moveWithSlope i _ []          _           = i
moveWithSlope i x area@(s:ss) slope@(a,b) = moveWithSlope j (x+a) nextArea slope where
  j        = incrementIfIsTree i $ s!!x
  nextArea = drop b area

incrementIfIsTree :: Int -> Char -> Int
incrementIfIsTree i c = i + if c == tree then 1 else 0

main :: IO Int 
main = do
  content <- readFile "day3-data.txt"
  let allLines = lines content
  let cycledLines = map cycle allLines
  return $ moveWithSlope 0 0 cycledLines (3,1)

main_2 :: IO Int
main_2 = do
  content <- readFile "day3-data.txt"
  let cycledLines = map cycle $ lines content 
  return $ foldl ((*)) 1 $ map (moveWithSlope 0 0 cycledLines) slopes
