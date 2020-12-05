module BitSet where

import Data.Bits

-- A bit set is a field of 'boolean' values packed into a memory efficient 
-- representation
--
-- @note for now we use an Integer that is growable. For small BitSet,
-- with few very large index, another data structure might be more memory 
-- efficient.
data BitSet = BitSet Integer

instance Show BitSet where
  show = toHexStr 

--
--
toHexStr:: BitSet -> String
toHexStr (BitSet 0) = "0x0"
toHexStr (BitSet i) = "0x" ++ _toHexStringInt i 

_toHexStringInt :: Integer -> String
_toHexStringInt 0 = ""
_toHexStringInt i = _toHexStringInt (i `div` 16) ++ if j < 10 then show j 
                                                              else case j of 
                                                                10 -> "a"
                                                                11 -> "b"
                                                                12 -> "c"
                                                                13 -> "d"
                                                                14 -> "e"
                                                                15 -> "f"
 where j = i `mod` 16

toBinStr :: BitSet -> String
toBinStr (BitSet i) = _toBinStrinInt i

_toBinStrinInt :: Integer -> String
_toBinStrinInt 0 = ""
_toBinStrinInt i = _toBinStrinInt (i `div` 2) ++ if i `mod` 2 == 0 then "0" else "1"

-- must be preceded by 0x and be char between 0 through f
fromHexStr :: String -> Maybe BitSet
fromHexStr "" = Nothing
fromHexStr str = if (take 2 str) /= "0x" then Nothing
                                         else fmap BitSet (_fromHexStrInt $ tail $ tail str)

_fromHexStrInt :: String -> Maybe Integer
_fromHexStrInt ""         = Just 0
_fromHexStrInt [x]        = singleHexChar x
_fromHexStrInt str@(x:xs) = f <$> (singleHexChar x) <*> (_fromHexStrInt xs) where
  f a b = a*(16^(length str-1)) + b
 

fromBinStr :: String -> Maybe BitSet
fromBinStr str = fmap BitSet (_fromBinStrInt str)

_fromBinStrInt :: String -> Maybe Integer
_fromBinStrInt ""         = Just 0
_fromBinStrInt [x]        = singleBinChar x
_fromBinStrInt str@(x:xs) = f <$> (singleBinChar x) <*> (_fromBinStrInt xs) where
  f a b = a*(2^(length str-1)) + b
    
singleBinChar  :: Char -> Maybe Integer
singleBinChar a = case a of
                      '0' -> Just  0
                      '1' -> Just  1
                      _   -> Nothing


singleHexChar :: Char -> Maybe Integer  
singleHexChar c =  case c of
                     '0' -> Just  0
                     '1' -> Just  1
                     '2' -> Just  2
                     '3' -> Just  3
                     '4' -> Just  4
                     '5' -> Just  5
                     '6' -> Just  6
                     '7' -> Just  7
                     '8' -> Just  8
                     '9' -> Just  9
                     'a' -> Just  10
                     'b' -> Just  11
                     'c' -> Just  12
                     'd' -> Just  13
                     'e' -> Just  14
                     'f' -> Just  15
                     _   -> Nothing


-- creates an empty bitset
create :: BitSet
create = BitSet 0

from :: Integral a => a -> BitSet
from i = BitSet $ toInteger i

get :: BitSet -> Int -> Bool
get (BitSet 0) _ = False 
get bs j = getAsInt bs j == 1

-- return 0 or 1
getAsInt :: BitSet -> Int -> Int
getAsInt (BitSet 0) _ = 0
getAsInt (BitSet i) j = fromIntegral $ (shiftR i j) .&. 1

set :: BitSet -> Int -> BitSet
set (BitSet i) j = BitSet (i .|. (shiftL 1 j))

getAll :: BitSet -> [Int] -> [Bool]
getAll bs is = map (\i -> get bs i) is

-- a list of 0 and 1
getAllAsInt :: BitSet -> [Int] -> [Int]
getAllAsInt bs is =  map (\i -> getAsInt bs i) is
