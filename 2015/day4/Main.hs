module Main where
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Crypto.Hash.MD5 as MD5
import Data.Char
import Data.Word

privateKey = "yzbqklnj"

hash :: String -> [Word8] 
hash str = Data.ByteString.unpack $ MD5.finalize ctx where
  ctx = MD5.update MD5.init (Data.ByteString.Char8.pack str)

hashLoop :: String -> String -> ([Word8]-> Bool) -> Integer -> Integer
hashLoop str key target i = if (target $ hash str) then i
                                                   else hashLoop (incrementPrefix str key) key target (i+1)

incrementPrefix :: String -> String -> String
incrementPrefix fullStr key = key ++ (show n)
  where n = (read (drop (length key) fullStr)) + 1

chechFiveLeadingZeros :: [Word8] -> Bool
checkFiveLeadingZeros (a:b:c:xs) = a == 0 && b == 0 && c < 16

checkSisLeadingZeros :: [Word8] -> Bool
checkSixLeadingZeros (a:b:c:xs) = a == 0 && b == 0 && c == 0

main = print $ hashLoop (privateKey ++ "0") privateKey checkFiveLeadingZeros 0 
