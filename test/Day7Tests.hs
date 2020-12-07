module Day7Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day7

testPart1 :: Assertion
testPart1 = assertEqual "" "OVXCKZBDEHINPFSTJLUYRWGAMQ" . part1 =<< readInput

testPart2 :: Assertion
testPart2 = assertEqual "" 955 . part2 =<< readInput

tests :: [TestTree]
tests = [
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
