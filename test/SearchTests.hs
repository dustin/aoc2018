module SearchTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.List             (sort, elemIndex)

import           Search

cycling :: Int -> Int -> [Int]
cycling a b = let (h,xs) = splitAt a [0..] in h <> cycle (take b xs)

prop_findCycle :: NonNegative (Small Int) -> Positive (Small Int) -> Bool
prop_findCycle (NonNegative (Small a)) (Positive (Small b)) = findCycle id (cycling a b) == (a,b)

-- Ended up not using this.
cyclen :: Eq a => [a] -> Maybe Int
cyclen []     = Nothing
cyclen [_]    = Nothing
cyclen (x:xs) = (1+) <$> (elemIndex x $ xs)

prop_cyclen :: NonNegative (Small Int) -> NonNegative (Small Int) -> Positive (Small Int) -> Bool
prop_cyclen (NonNegative (Small a)) (NonNegative (Small b)) (Positive (Small c)) =
  cyclen (drop (a + b) (cycling a c)) == Just c

propBinSearch :: Int -> Int -> Int -> Bool
propBinSearch a b c = let [a',b',c'] = sort [a,b,c] in
                        binSearch (flip compare b') a' c' == b'

tests :: [TestTree]
tests = [
  testProperty "find cycle" prop_findCycle,
  testProperty "cyclen" prop_cyclen,

  testProperty "bin search" propBinSearch
  ]
