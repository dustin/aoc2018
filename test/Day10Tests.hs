module Day10Tests where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Day10

testFindMin :: [TestTree]
testFindMin = map (\(f, xs, want) -> testCase (show xs) $ assertEqual "" want (findMin f xs)) [
  (id, [9], 9),
  (id, [9, 7, 5, 4, 3, 2, 3, 1], 2)
  ]

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

tests :: [TestTree]
tests = [
  testGroup "findMin" testFindMin,

  localOption (QC.QuickCheckTests 10000) $ testProperty "findMin" prop_min,
  localOption (QC.QuickCheckTests 10000) $ testProperty "findMax" prop_max,

  testProperty "findMin is not minimum" prop_findMinNotMin
  ]
