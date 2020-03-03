import Data.List

forbidden :: String -> [String] -> Bool
forbidden str []     = False
forbidden str (x:xs) = isInfixOf x str || forbidden str xs

doubleLetter :: String -> Bool
doubleLetter []     = False
doubleLetter [s]    = False
doubleLetter (x:xs) = if (x == head xs) then True
                                        else doubleLetter xs

containsVowels :: String -> Integer -> Bool
containsVowels []     i = i < 1
containsVowels (x:xs) i = if (x `elem` "aeiou") 
                            then containsVowels xs (i-1)
                            else containsVowels xs i

-- nice : 1, naughty : 0
nice :: String -> Int
nice str = if (not (forbidden str ["ab", "cd", "pq", "xy"]) && doubleLetter str && str `containsVowels` 3) 
                    then 1
                    else 0

main :: IO Int
main = do
  file <- readFile "day5-data.txt"
  return $ sum $ map nice $ lines file
