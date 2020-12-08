import Data.Set
import Data.List

getContent :: IO [String]
getContent = fmap lines $ readFile "day6-data.txt"

cleanData :: [String] -> [String]
cleanData ss = (intercalate "") <$> groupBy (\s1 s2 -> s2 /= "" ) ss

main :: IO ()
main = do
  content <- getContent
  print $ sum $ (length . fromList) <$> cleanData content

-- part 2
intersectAll :: Ord a => [Set a] -> Set a
intersectAll sets = foldl1 intersection sets

toSet :: [String] -> [Set Char]
toSet ss = fromList <$> ss

cleanData2 :: [String] -> [Set Char]
cleanData2 ss = intersectAll <$> toSet <$> keepNonEmpty <$> groupBy (\s1 s2 -> s2 /= "") ss

keepNonEmpty :: [String] -> [String]
keepNonEmpty ss = Data.List.filter ((/=) "") ss

main_2 :: IO ()
main_2 = do
  content <- getContent
  print $ sum $ length <$> cleanData2 content 


