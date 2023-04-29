module Parser (runParser, pJObject) where

import Data.Char (isDigit, digitToInt)
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
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \cs -> do
      (a, cs') <- p cs
      Just (f a, cs')

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \cs -> Just (a, cs)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser p) (Parser q) = Parser $ \cs -> do
      (f, cs') <- p cs
      (a, cs'') <- q cs'
      Just (f a, cs'')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser p) f = Parser $ \cs -> do
    (a, cs') <- p cs
    let (Parser q) = f a
    (b, cs'') <- q cs'
    Just (b, cs'')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p) (Parser q) = Parser $ \cs -> p cs <|> q cs

pChar :: Char -> Parser Char
pChar x = Parser g
  where
    g (c: cs)
      | c == x = Just (c, cs)
    g _ = Nothing

pString :: String -> Parser String
pString = traverse pChar

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser p
  where
    p (c : cs)
      | f c = Just (c, cs)
    p _ = Nothing

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

pJNull :: Parser JValue
pJNull = JNull <$ pString "null"

pJBool :: Parser JValue
pJBool = pTrue <|> pFalse
  where
    pTrue = JBool True <$ pString "true"
    pFalse = JBool False <$ pString "false"

pJNumber :: Parser JValue
pJNumber = JNumber . read <$> some (satisfy isDigit)

pQString :: Parser String
pQString = pChar '"' *>  many (satisfy $ \x -> x /= '"') <* pChar '"'

pJString :: Parser JValue
pJString = JString <$> pQString

pJLiteral :: Parser JValue
pJLiteral = pJNull <|> pJBool <|> pJNumber <|> pJString

pJValue :: Parser JValue
pJValue = JArray <$> (pChar '[' *> pJLiteral `sepBy` pChar ',' <* pChar ']') <|> pJLiteral

jsonEntry :: Parser (String, JValue)
jsonEntry = do
  key   <- pQString
  _     <- pChar ':'
  value <- pJValue
  return (key, value)

pJObject :: Parser JValue
pJObject = JObject <$> (pChar '{' *> many jsonEntry <* pChar '}')
