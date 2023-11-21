module HaskyJSON (parseJSON) where

import Parser (JValue, jsonObject, runParser)

parseJSON :: FilePath -> IO (Maybe (JValue, String))
parseJSON file = do
  content <- readFile file
  return $ runParser jsonObject content
