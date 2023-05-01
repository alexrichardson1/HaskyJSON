# HaskyJSON

HaskyJSON is a simple JSON parser for Haskell. It provides a simple `parseJSON` function that takes a file path as input, reads the content of the file, and returns the corresponding AST.

```haskell
import HaskyJSON (parseJSON)

main :: IO ()
main = do
  result <- parseJSON "path/to/json/file.json"
  case result of
      Just (jvalue, _) -> putStrLn $ "Parsed JSON value: " ++ show jvalue
      Nothing          -> putStrLn "Failed to parse JSON"
```