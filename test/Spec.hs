{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import qualified Day10Tests
import qualified Day11Tests
import qualified Day15Tests
import qualified Day16Tests
import qualified Day17Tests
import qualified Day18Tests
import qualified Day19Tests
import qualified Day20Tests
import qualified Day21Tests

tests :: [TestTree]
tests = [
  testGroup "day 10" Day10Tests.tests,
  testGroup "day 11" Day11Tests.tests,
  testGroup "day 15" Day15Tests.tests,
  testGroup "day 16" Day16Tests.tests,
  testGroup "day 17" Day17Tests.tests,
  testGroup "day 18" Day18Tests.tests,
  testGroup "day 19" Day19Tests.tests,
  testGroup "day 20" Day20Tests.tests,
  testGroup "day 21" Day21Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
