module Parsers where
import Control.Applicative
import Data.Char

-- thanks Toding :)
-- https://www.youtube.com/watch?v=N9RUqGYuGfw&t=2126s

newtype Parser a = Parser { runParser :: String -> Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)  
  (Parser f) <*> (Parser a) = Parser $ \input -> do
    (input',  f') <- f input
    (input'', x)  <- a input'
    Just (input'', f' x) 

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing 
  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input <|> p2 input


-- //////////
-- if the parser returns undefined, it fails instead
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs then Nothing
             else Just (input', xs)


-- //////////
-- parse characters until the predicate is false
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->  let (rest, token) = span f input in 
                                  Just (token, rest) where


-- //////////
-- Parser for a single character
charP :: Char -> Parser Char
charP c = Parser $ \s -> case s of
                           (x:xs) -> if x == c then Just (xs, c)
                                               else Nothing
                           _ -> Nothing


-- //////////
-- Parser for a String: array of Char
stringP :: String -> Parser String
stringP = sequenceA . map charP 


-- //////////
-- Parse a token: value before space
tokenP :: Parser String
tokenP = spanP (\c -> c /= ' ')


-- //////////
-- Parse a white space
ws :: Parser String
ws = stringP " "

-- //////////
-- Parse an Optional white space
optWs :: Parser String
optWs = spanP (\c -> c == ' ') 

-- //////////
-- Parser an integer
intP :: Parser Int
intP = (read <$> (notNull (spanP isDigit))) <|> negIntP

negIntP :: Parser Int
negIntP = (0-) <$> (charP '-' *> (read <$> (notNull (spanP isDigit)))) 

-- //////////
--
splitP :: Parser a -> Parser b -> Parser [b]
splitP sep element = (:) <$> element <*> many (sep *> element) <|> pure []
