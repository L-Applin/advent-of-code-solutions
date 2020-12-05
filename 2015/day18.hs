module Day18 where

import qualified BitSet
import Data.Maybe

type Size = (Int, Int)
data Action = Die | Born | Survive 
data State = Alive | Dead
type SideCheck = Int -> Size -> Bool
type Rule = Int -> Action

test :: BitSet.BitSet
test = BitSet.from 0x0ff0

testSize :: Size
testSize = (4, 4)

neighbors :: BitSet.BitSet -> Size -> Int -> Int
neighbors bs p@(x, y) i = (tl + t + tr + l + r + bl + b + br) where
  tl = if (isLeft i p) || (isTop i p)     then 0 else BitSet.getAsInt bs (i - x - 1)
  t  = if (isTop i p)                     then 0 else BitSet.getAsInt bs (i - x)
  tr = if (isTop i p) || (isRight i p)    then 0 else BitSet.getAsInt bs (i - x + 1)
  l  = if (isLeft i p)                    then 0 else BitSet.getAsInt bs (i - 1) 
  r  = if (isRight i p)                   then 0 else BitSet.getAsInt bs (i + 1)
  bl = if (isBottom i p) || (isLeft i p)  then 0 else BitSet.getAsInt bs (i + x - 1) 
  b  = if (isBottom i p)                  then 0 else BitSet.getAsInt bs (i + x) 
  br = if (isBottom i p) || (isRight i p) then 0 else BitSet.getAsInt bs (i + x + 1) 


isTop :: SideCheck
isTop i (x,_) = i < x
 
isLeft :: SideCheck
isLeft i (x, _) = i `mod` x == 0 

isRight :: SideCheck 
isRight i (x, _) = (i + 1) `mod` x == 0

isBottom :: SideCheck
isBottom i (x, y) = i >= (x*(y-1))

-- receive 1 or 0 as the state current state 
-- of the cell and the number of alive neighbors
-- and returns the new state of the cell
update :: Int -> Int -> Int
update 0 i = if i == 3           then 1 else 0
update 1 i = if i == 2 || i == 3 then 1 else 0


updageGrid :: BitSet -> Size -> Int -> BitSet
updageGrid grid size i = BitSet.set i 
