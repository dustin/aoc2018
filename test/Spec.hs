{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Day10 (findMin)

testFindMin :: [TestTree]
testFindMin = map (\(f, xs, want) -> testCase (show xs) $ assertEqual "" want (findMin f xs)) [
  (id, [9], 9),
  (id, [9, 7, 5, 4, 3, 2, 3, 1], 2)
  ]

-- Every value before the min value should be greater than the min value.
prop_min :: (NonEmptyList Int) -> Bool
prop_min f (NonEmpty xs) = let n = findMin id $ xs in
                              and . (\l -> zipWith (>=) l $ tail l) . takeWhile (/= n) $ xs

tests :: [TestTree]
tests = [
  testGroup "findMin" testFindMin,

  localOption (QC.QuickCheckTests 10000) $ testProperty "findMin" prop_min
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
