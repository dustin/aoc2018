module SearchTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.List             (sort)

import           Search

propBinSearch :: Int -> Int -> Int -> Bool
propBinSearch a b c = let [a',b',c'] = sort [a,b,c] in
                        binSearch (flip compare b') a' c' == b'

tests :: [TestTree]
tests = [
  testProperty "bin search" propBinSearch
  ]
