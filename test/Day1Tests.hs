module Day1Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day1

testPart1 :: Assertion
testPart1 = assertEqual "" 576 =<< sum <$> getInput

testPart2 :: Assertion
testPart2 = assertEqual "" 77674 =<< part2h <$> getInput

tests :: [TestTree]
tests = [
  testCase "part 1" testPart1,
  testCase "part 2" testPart2
  ]
