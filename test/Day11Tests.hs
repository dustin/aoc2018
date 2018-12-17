module Day11Tests where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import qualified Data.Array.Unboxed as A

import Day11

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


tests :: [TestTree]
tests = [ testSAT ]
