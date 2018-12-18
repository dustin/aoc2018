{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import qualified Day10Tests
import qualified Day11Tests
import qualified Day15Tests
import qualified Day17Tests
import qualified Day18Tests

tests :: [TestTree]
tests = [
  testGroup "day 10" Day10Tests.tests,
  testGroup "day 11" Day11Tests.tests,
  testGroup "day 15" Day15Tests.tests,
  testGroup "day 17" Day17Tests.tests,
  testGroup "day 18" Day18Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
