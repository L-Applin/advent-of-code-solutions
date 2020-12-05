module Day8 where
import Data.List

escapeQuote :: String
escapeQuote = "\\\""
escapeSlash :: String
escapeSlash = "\\\\"
escapeHex :: String
escapeHex = "\\x"

replace :: String -> String ->String -> String 
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)

replaceHex :: String -> String
replaceHex [] = []
replaceHex s = if (take 2 s) == "\\x" then "_" ++ replaceHex (drop 4 s)
                                      else (head s):(replaceHex $ tail s)


                       

main :: IO Int
main = do 
  file <- readFile "day8-data.txt"
  let words = lines file
  let word = (tail . init) $ words!!2
  let fixed = replace (replace (replaceHex word ) "\\\"" "_") "\\\\" "_"
  let l = (length word, length fixed)
  return $ length word - length fixed
