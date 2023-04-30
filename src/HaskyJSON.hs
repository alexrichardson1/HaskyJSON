module HaskyJSON (parseJSON) where

import System.IO  
import Control.Monad
import Parser

parseJSON :: FilePath -> IO (Maybe (JValue, String))
parseJSON file = do
    content <- readFile file
    return $ runParser jsonObject content