module Parser (runParser, jsonObject, JValue) where

import Data.Char (isDigit, isSpace)
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String)}

data JValue = JNull
            | JBool Bool
            | JNumber Int
            | JString String
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Show, Eq)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \cs -> do
      (a, cs') <- p cs
      Just (f a, cs')

instance Applicative Parser where
  pure a = Parser $ \cs -> Just (a, cs)

  (<*>) (Parser p) (Parser q) = Parser $ \cs -> do
      (f, cs') <- p cs
      (a, cs'') <- q cs'
      Just (f a, cs'')

instance Monad Parser where
  (>>=) (Parser p) f = Parser $ \cs -> do
    (a, cs') <- p cs
    let (Parser q) = f a
    (b, cs'') <- q cs'
    Just (b, cs'')

instance Alternative Parser where
  empty = Parser $ const Nothing

  (<|>) (Parser p) (Parser q) = Parser $ \cs -> p cs <|> q cs

char :: Char -> Parser Char
char x = Parser g
  where
    g (c: cs)
      | c == x = Just (c, cs)
    g _ = Nothing

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
sepBy p sep = (:) <$> p <*> many (sep *> p)

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
quoteString = lexer $ char '"' *>  many (satisfy $ \x -> x /= '"') <* char '"'

jsonString :: Parser JValue
jsonString = lexer $ JString <$> quoteString

jsonLiteral :: Parser JValue
jsonLiteral = lexer $ jsonNull <|> jsonBool <|> jsonNumber <|> jsonString

jsonValue :: Parser JValue
jsonValue = lexer $ JArray <$> (char '[' *> jsonLiteral `sepBy` char ',' <* char ']') <|> jsonLiteral

jsonEntry :: Parser (String, JValue)
jsonEntry = lexer $ do
  key   <- quoteString
  _     <- char ':'
  value <- jsonValue
  return (key, value)

jsonObject :: Parser JValue
jsonObject = lexer $ JObject <$> (char '{' *> many jsonEntry <* char '}')
