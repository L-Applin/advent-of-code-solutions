input = "3113322113"


takeUntilDiff :: String -> String
takeUntilDiff []     = []
takeUntilDiff [a]    = [a]
takeUntilDiff (x:xs) = if (x == head xs) then x:(takeUntilDiff xs)
                                         else [x]

splitOnDiff :: String -> (String, String)
splitOnDiff s = (take n s, drop n s) where
  n = length $ takeUntilDiff s

convert :: String -> String
convert []   = []
convert [a]  = '1':[a]
convert s = (show $ length s) ++ [head s]

convertAll :: String -> String
convertAll [] = []
convertAll s  = convert f ++ convertAll r where
  (f, r) = splitOnDiff s

applyN :: (a -> a) -> a -> Int -> a
applyN f a 0 = a
applyN f a i = applyN f (f a) (i - 1)

main :: IO Int
main = return $ length $ applyN convertAll input 50


