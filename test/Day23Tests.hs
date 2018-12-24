module Day23Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.Ingredients.Basic (HideSuccesses(..))

import           Day23

testBotDistance :: [TestTree]
testBotDistance = map (\(t, want) -> testCase (show t) $ assertEqual "" want (botDistance (Nanobot (0,0,0) 4) t)) [
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

testBotRange :: [TestTree]
testBotRange = map (\(t, want) -> testCase (show t) $ assertEqual "" want (inRange (Nanobot (0,0,0) 4) t)) [
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
testPart1 = do
  (Right p) <- getInput
  assertEqual "in range" 950 $ part1' p

testPart2 :: Assertion
testPart2 = do
  (Right p) <- getInput
  assertEqual "best point" 86871407 $ part2' p

tests :: [TestTree]
tests = [
  localOption (HideSuccesses True) $ testGroup "distance" testBotDistance,
  localOption (HideSuccesses True) $ testGroup "range" testBotRange,

  testCase "part 1" testPart1,
  testCase "part 2" testPart2
  ]
