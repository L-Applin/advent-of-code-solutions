module Day7 where
import Parsers 

import Data.Bits
import Data.Map (Map, insert)
import qualified Data.Map as Map
import Data.List.Split
import Data.Maybe
import Data.List
import Control.Applicative
import Text.Read

data Gate = AndGate  String String String
          | OrGate   String String String
          | LSGate   String Int String
          | RSGate   String Int String
          | NotGate  String String
          | ConsGate Int String
          | WireGate String String
          deriving (Show)

instance Eq Gate where
  g1 == g2 = output g1 == output g2
instance Ord Gate where
  g1 <= g2 = output g1 <= output g2

data Expr = And Expr Expr
          | Or Expr Expr
          | LShift Expr Int
          | RShift Expr Int
          | Not Expr
          | Cons Int
          | Wire Expr
          deriving (Show)

type Env = Map String Int

output :: Gate -> String
output (AndGate _ _ s) = s
output (OrGate  _ _ s) = s
output (LSGate  _ _ s) = s
output (RSGate  _ _ s) = s
output (NotGate _ s)   = s
output (ConsGate _ s)  = s
output (WireGate _ s)  = s

-- //////////
-- -- evaluation of the result (Int) of a Gate
eval :: Expr -> Int
eval (Cons i)     = i
eval (Not e)      = complement $ eval e
eval (RShift e i) = shiftR (eval e) i
eval (LShift e i) = shiftL (eval e) i
eval (Or e1 e2)   = eval e1 .|. eval e2
eval (And e1 e2)  = eval e1 .&. eval e2
eval (Wire e)     = eval e


-- //////////
-- Transform a Gate into an expression that can be evaluated. Needs
-- to lookup the input gates into anb environnement containing all 
-- of the gates definitions
transform :: Gate -> Map String Gate -> Map String Expr -> Expr
transform (ConsGate i _) _ _ = Cons i 
transform (AndGate input1 input2 output) gates exprs = case Map.lookup output exprs of
  Just e -> e
  Nothing -> binaryOp And input1 input2 gates exprs
transform (OrGate  input1 input2 output) gates exprs = binaryOp Or input1 input2 gates exprs
transform (LSGate  input shift output) gates exprs = 
  LShift (transform (safeLookup input gates) gates exprs) shift
transform (RSGate  input shift output) gates exprs = 
  RShift (transform (safeLookup input gates) gates exprs) shift
transform (NotGate input output) gates exprs = 
  Not (transform (safeLookup input gates) gates exprs)
transform (WireGate s output) gates exprs = 
  Wire (transform (safeLookup s gates) gates exprs)
  

binaryOp :: (Expr -> Expr -> Expr) -> String -> String -> Map String Gate -> Map String Expr -> Expr
binaryOp op input1 input2 gates exprs = op (transform g1 gates exprs) op2 where
  g1 = safeRead input1
  op2 = transform g2 gates exprs'
  g2 = safeRead input2
  safeRead input = case readMaybe input of
                     Just i  -> ConsGate i "_"
                     Nothing -> safeLookup input gates 
  exprs' = Map.insert input1 op2 exprs 


safeLookup :: String -> Map String Gate -> Gate
safeLookup s m = case Map.lookup s m of
                  Just g  -> g
                  Nothing -> error $ "Could not find gate " ++ s

-- ===============
-- parsing section
-- ===============

-- //////////
-- Take a Parser for a valid and gate string and converts it to a Parser Gate
arrowP :: Parser String
arrowP = stringP "->"

andGateParser :: Parser Gate
andGateParser = f <$> sequenceA [tokenP, ws, stringP "AND", ws, tokenP, ws, arrowP, ws, tokenP] where
  f xs = AndGate (xs!!0) (xs!!4) (xs!!8)

orGateParser :: Parser Gate
orGateParser = f <$> sequenceA [tokenP, ws, stringP "OR", ws, tokenP, ws, arrowP, ws, tokenP] where
  f xs = OrGate (xs!!0) (xs!!4) (xs!!8)

leftShiftGateParser :: Parser Gate
leftShiftGateParser = f <$> sequenceA [tokenP, ws, stringP "LSHIFT", ws, tokenP, ws, arrowP, ws, tokenP] where
  f xs = LSGate (xs!!0) (read $ xs!!4) (xs!!8) 

rightShiftGateParser :: Parser Gate
rightShiftGateParser = f <$> sequenceA [tokenP, ws, stringP "RSHIFT", ws, tokenP, ws, arrowP, ws, tokenP] where
  f xs = RSGate (xs!!0) (read $ xs!!4) (xs!!8)

notGateParser :: Parser Gate
notGateParser = f <$> sequenceA [stringP "NOT", ws, tokenP, ws, arrowP, ws, tokenP] where
  f xs = NotGate (xs!!2) (xs!!6)

consGateParser :: Parser Gate
consGateParser = f <$> sequenceA [tokenP, ws, arrowP, ws, tokenP] where
  f xs = case readMaybe (xs!!0) of
           Just i  -> ConsGate i       (xs!!4)
           Nothing -> WireGate (xs!!0) (xs!!4) 



gateParser :: Parser Gate
gateParser = consGateParser 
         <|> andGateParser 
         <|> orGateParser
         <|> leftShiftGateParser 
         <|> rightShiftGateParser 
         <|> notGateParser 


main :: IO Int 
main = do
  file <- readFile "day7-data.txt"
  let gates = map f $ lines file where f x = (snd . fromJust) $ runParser gateParser x
  let env = Map.fromList $ map (\g -> (output g, g)) $ reverse $ sort gates
  print env
  print "Start transform"
  let e = transform (safeLookup "dq" env) env Map.empty
  print "start eval"
  -- print e
  return $ eval e

