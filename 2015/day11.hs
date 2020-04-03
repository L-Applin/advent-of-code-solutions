import Data.Char

type PasswordRule = String -> Bool

input :: String
input  = "hxbxwxba"

incChar :: Char -> Char
incChar c   = chr (ord c + 1)

incWord :: String -> String
incWord []  = []
incWord [c] = case c of 
                'z' -> "aa" 
                _   -> [incChar c]
incWord str = case (last str) of
                'z' -> incWord (init str) ++ ['a']
                c   -> init str ++ [incChar c]

checkPassword :: String -> [PasswordRule] -> Bool
checkPassword str []    = True
checkPassword str rules = (head rules) str && checkPassword str (tail rules) 

threeConsecutive :: PasswordRule
threeConsecutive []  = False
threeConsecutive str = foldl (||) False
                       $ map (\t -> (t!!0)+1 == t!!1 && (t!!2)-1 == t!!1) 
                       $ map (map ord) $ splitThree str 

splitThree :: String -> [String]
splitThree []        = []
splitThree [a]       = []
splitThree [a,b]     = []
splitThree str       = (splitThree $ tail str) ++ [take 3 str]

illegalChar :: String -> PasswordRule
illegalChar illegal str = not $ foldl (||) False $ map (\c -> c `elem` str) illegal


doubleChar :: PasswordRule
doubleChar []  = False
doubleChar [a] = False
doubleChar str = found && fst (singleDoubleChar rest) where
  (found, rest) = singleDoubleChar str
                   
singleDoubleChar :: String -> (Bool, String)
singleDoubleChar []     = (False, "")
singleDoubleChar [a]    = (False, "")
singleDoubleChar (x:xs) = if (x == head xs) then (True, tail xs)
                                            else singleDoubleChar xs

findNextPassword :: String -> String
findNextPassword password = if (checkPassword password [doubleChar, threeConsecutive, illegalChar "iol"]) then password
                                                                                                          else findNextPassword $ incWord password

main :: IO String
main = return $ findNextPassword $ incWord $ findNextPassword input
