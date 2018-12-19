module Day19Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.List             (nub)

import           Day19

prop_DivisorsDivide :: Positive Int -> Bool
prop_DivisorsDivide (Positive x) = all (\n -> x `mod` n == 0) (divisors x)

prop_DivisorsUnique :: Positive Int -> Bool
prop_DivisorsUnique (Positive x) = divisors x == nub (divisors x)

testPart1 :: Assertion
testPart1 = assertEqual "regs" (2520,865,1,865,256,864) =<< evalp
  where evalp = do
          (Right p) <- getInput
          pure $ execute p 0 (0,0,0,0,0,0)

testPart2 :: Assertion
testPart2 = assertEqual "" 27941760 $ sum (divisors 10551264)

tests :: [TestTree]
tests = [
  testGroup "divisors" [
      testProperty "divide" prop_DivisorsDivide,
      testProperty "unique" prop_DivisorsUnique
      ],

  testCase "part 1" testPart1,
  testCase "part 2" testPart2
  ]
