module Main (main) where

import Test.HUnit
import Parser
import qualified System.Exit as Exit

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

tests :: Test
tests = TestList [TestLabel "testPrimitive" testPrimitive]