countRecur :: String -> Int -> Int
countRecur "" i = i
countRecur (x:xs) i 
  | x == '('  = countRecur xs (i+1)
  | x == ')'  = countRecur xs (i-1)
  | otherwise = countRecur xs i

countParent :: String -> String
countParent s = show $ countRecur s 0

main = interact countParent
