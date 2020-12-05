import Parsers

data Password = Password { req_range :: (Int, Int)
                         , req :: Char
                         , pwd :: String
                         } deriving (Show, Eq)

-- functions for parsing for each line
-- format is :    
--     
--     n-m c: <actuall password>
-- 
-- where :
--  n-m is the required range, ie 4-8
--  c   is the character to check
--  <actual password> is the actuall password that need to be checked
pwdParser :: Parser Password
pwdParser = Password <$> rangeParser <*> (charP ' ' *> requirementParser <* charP ' ') <*> tokenP

rangeParser :: Parser (Int, Int)
rangeParser = arrToTup <$> splitP (charP '-') intP 

arrToTup :: [x] -> (x, x)
arrToTup (x:xs) = (x, head xs)

requirementParser :: Parser Char
requirementParser = fmap head $ spanP (\c -> c /= ':') <* charP ':'


-- the actuall check if passdword is valid
validatePwd :: Password -> Bool
validatePwd (Password (n,m) c p) = i >= n && i <= m where
  i = foldl (\j s -> if c == s then j + 1 else j) 0 p

countBools :: [Maybe Bool] -> Int
countBools bs = length $ filter f bs where
  f (Just b) = b
  f Nothing  = False

doMain :: (Password -> Bool) -> IO Int
doMain validator = do
  content <- readFile "day2-data.txt"
  let parsingRes = map (runParser pwdParser) $ lines content
  let maybeBools = map (fmap $ validator . snd) parsingRes
  return $ countBools maybeBools 
  
main_1 :: IO Int
main_1 = doMain validatePwd

-- part 2
xor :: Bool -> Bool -> Bool
xor a b = (not a && b) || (a && not b) 

validatePwdPosition :: Password -> Bool
validatePwdPosition (Password (n,m) c p) = first `xor` second where
  first  = p!!(n-1) == c
  second = p!!(m-1) == c 

main :: IO Int
main = doMain validatePwdPosition 
