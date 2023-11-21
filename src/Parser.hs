module Parser (runParser, jsonObject, JValue (..)) where

import Control.Applicative
import Data.Char (isDigit, isSpace)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

data JValue
  = JNull
  | JBool Bool
  | JNumber Int
  | JString String
  | JArray [JValue]
  | JObject [(String, JValue)]
  deriving (Show, Eq)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (a, rs) <- p input
    Just (f a, rs)

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)

  (<*>) (Parser p) (Parser q) = Parser $ \input -> do
    (f, rs) <- p input
    (a, rs') <- q rs
    Just (f a, rs')

instance Monad Parser where
  (>>=) (Parser p) f = Parser $ \input -> do
    (a, rs) <- p input
    let (Parser q) = f a
    (b, rs') <- q rs
    Just (b, rs')

instance Alternative Parser where
  empty = Parser $ const Nothing

  (<|>) (Parser p) (Parser q) = Parser $ \input -> p input <|> q input

char :: Char -> Parser Char
char x = Parser p
  where
    p (c : cs)
      | c == x = Just (c, cs)
    p _ = Nothing

string :: String -> Parser String
string = traverse char

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser p
  where
    p (c : cs)
      | f c = Just (c, cs)
    p _ = Nothing

spaces :: Parser String
spaces = many $ satisfy isSpace

lexer :: Parser a -> Parser a
lexer parser = spaces *> parser <* spaces

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = ((:) <$> p <*> many (sep *> p)) <|> pure []

jsonNull :: Parser JValue
jsonNull = lexer $ JNull <$ string "null"

jsonBool :: Parser JValue
jsonBool = lexer $ pTrue <|> pFalse
  where
    pTrue = JBool True <$ string "true"
    pFalse = JBool False <$ string "false"

jsonNumber :: Parser JValue
jsonNumber = lexer $ JNumber . read <$> some (satisfy isDigit)

quoteString :: Parser String
quoteString = lexer $ char '"' *> many (satisfy $ \x -> x /= '"') <* char '"'

jsonString :: Parser JValue
jsonString = lexer $ JString <$> quoteString

jsonPrimitive :: Parser JValue
jsonPrimitive = lexer $ jsonNull <|> jsonBool <|> jsonNumber <|> jsonString

jsonValue :: Parser JValue
jsonValue = lexer $ jsonObject <|> JArray <$> (char '[' *> (jsonObject <|> jsonValue <|> jsonPrimitive) `sepBy` char ',' <* char ']') <|> jsonPrimitive

jsonEntry :: Parser (String, JValue)
jsonEntry = lexer $ do
  key <- quoteString
  _ <- char ':'
  value <- jsonValue
  return (key, value)

jsonObject :: Parser JValue
jsonObject = lexer $ JObject <$> (char '{' *> jsonEntry `sepBy` char ',' <* char '}')
