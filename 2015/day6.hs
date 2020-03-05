module Day6 where
import Data.List.Split
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

-- data representation
type Pos = (Int, Int)
type Grid = IntSet
data ActionType = Toggle 
                | SwitchOn 
                | SwitchOff 
                | None 
                deriving (Show, Eq)
data Action = Action { actionType :: ActionType
                     , start      :: Pos
                     , end        :: Pos  
                     } deriving (Show, Eq)

-- parsing
-- assumes data is like "turn on 0,0 through 999,999"
parseAction :: String -> Action
parseAction []  = Action None (0,0) (0,0) -- empty String, do nothing, should never happend \kappa\
parseAction str = Action aType posStart posEnd where
                        aType    = parseActionType strSplit 
                        posStart = tup2 $ strSplit !! (length strSplit - 3)
                        posEnd   = tup2 $ strSplit !! (length strSplit - 1)
                        strSplit = splitOn " " str

tup2 :: String -> Pos
tup2 str = (a, b) where
  a = read $ split !! 0
  b = read $ split !! 1
  split = splitOn "," str

parseActionType :: [String] -> ActionType
parseActionType strs = case (head strs) of
                         "turn"   -> if strs !! 1 == "on" then SwitchOn else SwitchOff
                         "toggle" -> Toggle
                         _        -> None

gridUpdate :: IntSet -> Action -> IntSet
gridUpdate grid Action{actionType=SwitchOn, start=s, end=e} =  grid `IntSet.union` (createAllIndices s e) -- add grid $ createAllIndices s e where
gridUpdate grid Action{actionType=SwitchOff, start=s, end=e} = grid IntSet.\\ (createAllIndices s e) -- remove grid $ createAllIndices s e where
gridUpdate grid Action{actionType=Toggle, start=s, end=e} = (grid IntSet.\\ indices) `IntSet.union` (indices IntSet.\\ grid) where -- Symmetric Difference
     indices = createAllIndices s e

createIndices :: (Int, Int) -> (Int, Int) -> IntSet
createIndices (a, b) (c, d) = IntSet.fromList [(a+b*1000)..(c+b*1000)]

createAllIndices :: (Int, Int) -> (Int, Int) -> IntSet
createAllIndices (a, b) (c, d) = if b==d then createIndices (a, b) (c, d) 
                                         else (createIndices (a, b) (c, d)) `IntSet.union` (createAllIndices (a, b+1) (c, d))
reduceActions :: [Action] -> IntSet
reduceActions as = foldl gridUpdate IntSet.empty as

main :: IO Int
main = do
  actions <- readFile "day6-data.txt"
  return $ IntSet.size $ reduceActions $ map parseAction $ lines actions

