module HaskyJSON (parseJSON) where

import           Parser

parseJSON :: FilePath -> IO (Maybe (JValue, String))
parseJSON file = do
    content <- readFile file
    return $ runParser jsonObject content
