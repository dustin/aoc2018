module Day18Tests where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Day18

prop_findCycle :: NonNegative (Small Int) -> Positive (Small Int) -> Bool
prop_findCycle (NonNegative (Small a)) (Positive (Small b)) =
  let (h,xs) = splitAt a [0..] in
    findCycle id (h <> cycle (take b xs)) == (a,b)

testPart1 :: Assertion
testPart1 = assertEqual "score" 467819 =<< score . tx 10 <$> getInput

testPart2 :: Assertion
testPart2 = assertEqual "score" 195305 =<< part2' <$> getInput

tests :: [TestTree]
tests = [
  testProperty "find cycle" prop_findCycle,

  testCase "part 1" testPart1,
  testCase "part 2" testPart2
  ]
