module Main (main) where

import           Parser
import qualified System.Exit as Exit
import           Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess

testPrimitive :: Test
testPrimitive = TestList [TestCase (assertEqual "null" (Just (JObject [("x", JNull)], "")) (runParser jsonObject "{\"x\":null}")),
                          TestCase (assertEqual "true" (Just (JObject [("x", JBool True)], "")) (runParser jsonObject "{\"x\":true}")),
                          TestCase (assertEqual "false" (Just (JObject [("x", JBool False)], "")) (runParser jsonObject "{\"x\":false}")),
                          TestCase (assertEqual "int" (Just (JObject [("x", JNumber 0)], "")) (runParser jsonObject "{\"x\":0}"))
                         ]

testArray :: Test
testArray = TestList [TestCase (assertEqual "empty" (Just (JObject [("x", JArray [])], "")) (runParser jsonObject "{\"x\":[]}")),
                      TestCase (assertEqual "primitives" (Just (JObject [("x", JArray [JNumber 1, JNumber 2, JNumber 3])], "")) (runParser jsonObject "{\"x\":[1,2,3]}")),
                      TestCase (assertEqual "object" (Just (JObject [("x", JArray [JObject [("y", JNumber 0)]])], "")) (runParser jsonObject "{\"x\":[{\"y\":0}]}")),
                      TestCase (assertEqual "nest array" (Just (JObject [("x", JArray [JNumber 1, JNumber 2, JArray [JNumber 3, JNumber 4]])], "")) (runParser jsonObject "{\"x\":[1,2,[3,4]]}"))
                     ]

testObject :: Test
testObject = TestList [TestCase (assertEqual "empty" (Just (JObject [], "")) (runParser jsonObject "{}")),
                       TestCase (assertEqual "object" (Just (JObject [("x", JNull), ("y", JNull)], "")) (runParser jsonObject "{\"x\":null,\"y\":null}")),
                       TestCase (assertEqual "nest object" (Just (JObject [("x", JObject [("y", JNull)])], "")) (runParser jsonObject "{\"x\":{\"y\":null}}"))
                      ]

tests :: Test
tests = TestList [TestLabel "testPrimitive" testPrimitive, TestLabel "testArray" testArray, TestLabel "testObject" testObject]
