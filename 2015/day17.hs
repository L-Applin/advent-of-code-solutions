module Day17 where

containers :: [Int]
containers = [43, 3, 4, 10, 21, 44, 4, 6, 47, 41, 34, 17, 17, 44, 36, 31, 46, 9, 27, 38]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
 | n < 0     = []
 | otherwise = case drop (n-1) xs of
                 [ ] -> []
                 [_] -> [xs]
                 _   -> [y:c | c <- combinations (n-1) ys] ++ combinations n ys

check :: Int -> [Int] -> Bool
check i xs = sum xs == i

checkAll :: Int -> [[Int]] -> Int
checkAll i xs = sum $ map (\is -> if check i is then 1 else 0) xs

main :: Int
main = sum $ map f [4..(length containers)] where
  f i = checkAll 150 (combinations i containers)


                                                                                                    
                                                                                                    
