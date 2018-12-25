module Day23Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day23

testBotDistance :: Assertion
testBotDistance = mapM_ (\(t, want) -> assertEqual (show t) want (botDistance (Nanobot (0,0,0) 4) t)) [
  (Nanobot (0,0,0) 4, 0),
  (Nanobot (1,0,0) 1, 1),
  (Nanobot (4,0,0) 3, 4),
  (Nanobot (0,2,0) 1, 2),
  (Nanobot (0,5,0) 3, 5),
  (Nanobot (0,0,3) 1, 3),
  (Nanobot (1,1,1) 1, 3),
  (Nanobot (1,1,2) 1, 4),
  (Nanobot (1,3,1) 1, 5)
  ]

testBotRange :: Assertion
testBotRange = mapM_ (\(t, want) -> assertEqual (show t) want (inRange (Nanobot (0,0,0) 4) t)) [
  (Nanobot (0,0,0) 4, True),
  (Nanobot (1,0,0) 1, True),
  (Nanobot (4,0,0) 3, True),
  (Nanobot (0,2,0) 1, True),
  (Nanobot (0,5,0) 3, False),
  (Nanobot (0,0,3) 1, True),
  (Nanobot (1,1,1) 1, True),
  (Nanobot (1,1,2) 1, True),
  (Nanobot (1,3,1) 1, False)
  ]

testPart1 :: Assertion
testPart1 = assertEqual "in range" 950 =<< part1' <$> getInput

testPart2 :: Assertion
testPart2 = assertEqual "best point" 86871407 =<< part2' <$> getInput

tests :: [TestTree]
tests = [
  testCase "distance" testBotDistance,
  testCase "range" testBotRange,

  testCase "part 1" testPart1,
  testCase "part 2" testPart2
  ]
