import qualified Data.Map as Map
import Data.List.Split
import Data.List

type Sue = (Int, [(String, Int)])

mfcsam :: [(String, Int)]
mfcsam = [
           ("children", 3),
           ("cats", 7),
           ("samoyeds", 2),
           ("pomeranians", 3),
           ("akitas", 0),
           ("vizslas", 0),
           ("goldfish", 5),
           ("trees", 3),
           ("cars", 2),
           ("perfumes", 1)
         ]

parse :: String -> Sue
parse s = (i, m) where
  i = read $ case length (words!!1) of
        2 -> [head $ words!!1]
        _ -> init $ words!!1 
  m = [(init (words!!2), read [head (words!!3)]), (init (words!!4), read [head (words!!5)]), (init (words!!6), read [head (words!!7)])] 
  words = splitOn " " s 

parseAll :: String -> [Sue]
parseAll s = map parse $ lines s


lookupSue :: [(String, Int)] -> Sue -> Bool
lookupSue mfc (n, l) = foldl (&&) True $ map f l where 
  f (s, i) = case lookup s mfc of
             Just y  -> i == y
             Nothing -> error $ "not supported: " ++ s


solve :: [Sue] -> Maybe Int
solve sues = elemIndex True $ map (lookupSue mfcsam) sues

main :: IO (Maybe Int)
main = do 
  file <- readFile "day16-data.txt"
  return $ (+1) <$> (solve $ parseAll file)
