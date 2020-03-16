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

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f a = a
applyN 1 f a = f a
applyN i f a = applyN (i-1) f (f a)

main :: IO Int
main = return $ length $ applyN 50 convertAll input


