module Day25Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day25

testExampleConst :: Assertion
testExampleConst = mapM_ (\(fn,want) -> assertEqual fn want =<< nConstellations <$> getInput' fn) [
  ("input/day25.sample", 2),
  ("input/day25.sample2", 4),
  ("input/day25.sample3", 3),
  ("input/day25.sample4", 8)
  ]

-- 244 is too low
testPart1 :: Assertion
testPart1 = assertEqual "" 430 =<< nConstellations <$> getInput' "input/day25"

testPart2 :: Assertion
testPart2 = pure ()

tests :: [TestTree]
tests = [
  testCase "example const" testExampleConst,
  testCase "part 1" testPart1,
  testCase "part 2" testPart2
  ]
