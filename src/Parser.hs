module Parser where

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
  (<*>) (Parser p) (Parser q) = Parser (\cs -> do
      (f, cs') <- p cs
      (a, cs'') <- q cs'
      Just (f a, cs'')
    )

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p) (Parser q) = Parser $ \cs -> p cs <|> q cs

pChar :: Char -> Parser Char
pChar x = Parser g
  where
    g (c: cs)
      | c == x = Just (c, cs)
    g _ = Nothing

pString :: String -> Parser String
pString = sequenceA . map pChar

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser p
  where
    p (c : cs)
      | f c = Just (c, cs)
    p _ = Nothing

pDigit :: Parser Int
pDigit = digitToInt <$> satisfy isDigit

pNull :: Parser JValue
pNull = JNull <$ pString "null"

pBool :: Parser JValue
pBool = pTrue <|> pFalse
  where
    pTrue = JBool True <$ pString "true"
    pFalse = JBool False <$ pString "false"