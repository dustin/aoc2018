{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Data.List (sort)
import qualified Data.Array.Unboxed as A

import Day10 (findMin)
import Day11 (Grid(..), mkSumAreaTable, sumIn)
import qualified Day15
import qualified Day17

testFindMin :: [TestTree]
testFindMin = map (\(f, xs, want) -> testCase (show xs) $ assertEqual "" want (findMin f xs)) [
  (id, [9], 9),
  (id, [9, 7, 5, 4, 3, 2, 3, 1], 2)
  ]

msquare :: [[Int]]
msquare  = [[31,  2,  4, 33,  5, 36],
            [12, 26,  9, 10, 29, 25],
            [13, 17, 21, 22, 20, 18],
            [24, 23, 15, 16, 14, 19],
            [30,  8, 28, 27, 11,  7],
            [1,  35, 34,  3, 32,  6]]

msqarry :: Grid
msqarry = Grid $ A.array ((1,1),(6,6)) [((x,y), msquare !! (y-1) !! (x-1)) | x <- [1..6], y <- [1..6]]

-- (3,4) -> (5, 5)  --  (3,4)  (5,4)   (3,5) (5,5)
testSAT :: TestTree
testSAT = testCase "example" $  assertEqual "" 111 $ sumIn g' (3,4) (5,5)
  where g' = mkSumAreaTable msqarry

-- Every value before the min value should be greater than the min value.
prop_min :: (NonEmptyList Int) -> Bool
prop_min (NonEmpty xs) = let n = findMin id $ xs in
                           and . (\l -> zipWith (>=) l $ tail l) . takeWhile (/= n) $ xs

-- Every value before the min value should be less than the max value.
prop_max :: (NonEmptyList Int) -> Bool
prop_max (NonEmpty xs) = let n = findMax id $ xs in
                           and . (\l -> zipWith (<=) l $ tail l) . takeWhile (/= n) $ xs

-- I wrote this, but ended up not using it.  Figured since I have a
-- test, I'll leave it here.
findMax :: Ord b => (a -> b) -> [a] -> a
findMax f (x:xs) = go xs x
  where go [] r = r
        go (x':xs') r
          | f x' < f r = r
          | otherwise = go xs' x'


-- Verify findMin ≠ minimum by ensuring there's a small uptick and
-- then still yet lower value at the end of the list.
prop_findMinNotMin :: (NonEmptyList Int) -> Bool
prop_findMinNotMin (NonEmpty xs) = let mn = minimum xs
                                       l = xs <> [succ mn, (pred.pred) mn] in
                                     findMin id l /= minimum l

day15Test :: String -> Int -> Int -> Assertion
day15Test fn rounds score = do
  w <- Day15.parseInput . lines <$> readFile fn
  let (x,w') = Day15.play w 0 (Day15.mkHit 3)
  assertEqual "rounds" rounds x
  assertEqual "score" score (Day15.score w')


propReadingOrder :: (Int,Int) -> (Int,Int) -> Bool
propReadingOrder a b = Day15.readingOrder a b == ro a b
  where
    ro :: (Int,Int) -> (Int,Int) -> Ordering
    ro (x1,y1) (x2,y2)
      | y1 < y2 = LT
      | y1 == y2 = compare x1 x2
      | y1 > y2 = GT

propBinSearch :: Int -> Int -> Int -> Bool
propBinSearch a b c = let [a',b',c'] = sort [a,b,c] in
                        Day15.binSearch (flip compare b') a' c' == b'

run17 :: FilePath -> String -> IO ()
run17 inf outf = do
  (Right s) <- Day17.getInput' inf
  let s' = Day17.pour s
  writeFile outf (show s' <> "\n" <> show (Day17.countWater s', Day17.countWater2 s'))

tests :: [TestTree]
tests = [
  goldenVsFileDiff "day17 (flood)" (\ref new -> ["diff", "-qw", ref, new])
    "test/output/day17.txt" "test/output/,day17.txt" (run17 "input/day17" "test/output/,day17.txt"),

  testGroup "findMin" testFindMin,

  localOption (QC.QuickCheckTests 10000) $ testProperty "findMin" prop_min,
  localOption (QC.QuickCheckTests 10000) $ testProperty "findMax" prop_max,

  testProperty "findMin is not minimum" prop_findMinNotMin,

  testSAT,

  testProperty "reading order" propReadingOrder,
  testCase "day 15 s2" $ day15Test "input/day15.sample" 47 590,
  testCase "day 15 s2" $ day15Test "input/day15.sample2" 20 937,
  testCase "day 15 s3" $ day15Test "input/day15.sample3" 54 536,
  testCase "day 15 s4" $ day15Test "input/day15.sample4" 35 793,
  testCase "day 15 s4" $ day15Test "input/day15.sample5" 46 859,
  -- testCase "day 15 big" $ day15Test "input/day15.bigsample" 68 2812,

  testProperty "bin search" propBinSearch
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
