{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Day10 (findMin)

testFindMin :: [TestTree]
testFindMin = map (\(f, xs, want) -> testCase (show xs) $ assertEqual "" want (findMin f xs)) [
  (id, [9], 9),
  (id, [9, 7, 5, 4, 3, 2, 3], 2)
  ]

-- Every value before the min value should be greater than the min value.
prop_findMin :: (NonEmptyList Int) -> Bool
prop_findMin (NonEmpty xs) = let n = findMin id xs in
                               all (> n) . takeWhile (/= n) $ xs

tests :: [TestTree]
tests = [
  testGroup "findMin" testFindMin,

  testProperty "findMin" prop_findMin
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
