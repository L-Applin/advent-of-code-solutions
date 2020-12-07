import Data.Maybe
import Parsers
import Control.Applicative
import Data.List 
import Data.List.Split

data Passport = Passport {  byr::Maybe String                          
                          , iyr::Maybe String
                          , eyr::Maybe String
                          , hgt::Maybe String
                          , hcl::Maybe String
                          , ecl::Maybe String
                          , pid::Maybe String
                          , cid::Maybe String 
                          } deriving (Show, Eq)


createPassport :: [(String, String)] -> Passport
createPassport info = Passport 
                 (lookup "byr" info)
                 (lookup "iyr" info)
                 (lookup "eyr" info)
                 (lookup "hgt" info)
                 (lookup "hcl" info)
                 (lookup "ecl" info)
                 (lookup "pid" info)
                 (lookup "cid" info)

isValid :: Passport -> Bool
isValid p =    isJust (byr p) 
            && isJust (iyr p)
            && isJust (eyr p)
            && isJust (hgt p)
            && isJust (hcl p)
            && isJust (ecl p)
            && isJust (pid p)
            -- cid ommited

isValidWithValidation :: Passport -> Bool
isValidWithValidation p =    
               fromMaybe False (fmap validateByr $ byr p) 
            && fromMaybe False (fmap validateIyr $ iyr p)
            && fromMaybe False (fmap validateEyr $ eyr p)
            && fromMaybe False (hgt p >>= validateHgt)
            && fromMaybe False (fmap validateHcl $ hcl p)
            && fromMaybe False (fmap validateEcl $ ecl p)
            && fromMaybe False (fmap validatePid $ pid p)

validateByr :: String -> Bool
validateByr s = length s == 4 && y >= 1920 && y <= 2002 where y = read s

validateIyr :: String -> Bool
validateIyr s = length s == 4 && y >= 2010 && y <= 2020 where y = read s

validateEyr :: String -> Bool
validateEyr s = length s == 4 && y >= 2020 && y <= 2030 where y = read s
                                                            
validateHgt :: String -> Maybe Bool
validateHgt s = doValidate <$> input where
  doValidate (i,unit) = case unit of
                          "cm" -> i >= 150 && i <= 193
                          "in" -> i >= 59 && i <= 76
                          _    -> False
  input = fmap snd $ runParser heightParser s 
                                                           
validateHcl :: String -> Bool
validateHcl s = length s == 4 && y > 2020 && y < 2030 where y = read s

validateEcl :: String -> Bool
validateEcl s = length s == 4 && y > 2020 && y < 2030 where y = read s

validatePid :: String -> Bool
validatePid s = length s == 4 && y > 2020 && y < 2030 where y = read s

                                                            
-- parsing of passport info
heightParser :: Parser (Int, String)
heightParser = (,) <$> intP <*> (stringP "cm" <|> stringP "in")

passpInfoParser :: Parser (String, String)
passpInfoParser = (,) <$> spanP (\c -> c /= ':') <* charP ':' <*> tokenP 

passportParser :: Parser Passport
passportParser = createPassport <$> splitP (charP ' ') passpInfoParser


-- main
cleanData :: [String] -> [String]
cleanData d = map (intercalate " ") (map removeEmpty input) where
  removeEmpty arr = (filter (\s -> s /= "") arr) >>= (splitOn " ")
  input = groupBy (\s1 s2 -> s2 /= "") d

main :: IO () 
main = do
  content <- readFile "day4-data.txt"
  let passData = cleanData $ lines content 
  let passports = map (snd . fromJust . (runParser passportParser)) passData 
  print $ length $ filter isValid passports
  

