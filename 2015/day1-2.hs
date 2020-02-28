findBasementRecur :: String -> Int -> Int -> Int
findBasementRecur s (-1) n = n
findBasementRecur (c:cs) i n
  | c == '('  = findBasementRecur cs (i+1) (n+1)
  | c == ')'  = findBasementRecur cs (i-1) (n+1)
  | otherwise = findBasementRecur cs i (n+1)

findBasement :: String -> String
findBasement s = show $ findBasementRecur s 0 0

main = interact findBasement

