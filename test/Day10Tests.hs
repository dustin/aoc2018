module Day10Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day10

testPart2 :: Assertion
testPart2 = assertEqual "" 10880 =<< part2' <$> getInput

tests :: [TestTree]
tests = [
  testCase "part2" testPart2
  ]
