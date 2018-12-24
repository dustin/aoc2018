module Day15Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.List             (sort)

import           Day15

propReadingOrder :: (Int,Int) -> (Int,Int) -> Bool
propReadingOrder a b = readingOrder a b == ro a b
  where
    ro :: (Int,Int) -> (Int,Int) -> Ordering
    ro (x1,y1) (x2,y2)
      | y1 < y2 = LT
      | y1 == y2 = compare x1 x2
      | y1 > y2 = GT

day15Test :: String -> Int -> Int -> Assertion
day15Test fn rounds sc = do
  w <- parseInput . lines <$> readFile fn
  let (x,w') = play w 0 (mkHit 3)
  assertEqual "rounds" rounds x
  assertEqual "score" sc (score w')

tests :: [TestTree]
tests = [
  testProperty "reading order" propReadingOrder,
  testCase "day 15 s2" $ day15Test "input/day15.sample" 47 590,
  testCase "day 15 s2" $ day15Test "input/day15.sample2" 20 937,
  testCase "day 15 s3" $ day15Test "input/day15.sample3" 54 536,
  testCase "day 15 s4" $ day15Test "input/day15.sample4" 35 793,
  testCase "day 15 s4" $ day15Test "input/day15.sample5" 46 859
  -- testCase "day 15 big" $ day15Test "input/day15.bigsample" 68 2812, -- ~4s
  ]
