module JsonParser (parseJSON) where

import System.IO  
import Control.Monad

parseJSON :: FilePath -> IO ()
parseJSON file = do
    content <- readFile file
    putStrLn content