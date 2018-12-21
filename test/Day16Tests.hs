{-# LANGUAGE OverloadedStrings #-}

module Day16Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day16

testPart1 :: Assertion
testPart1 = do
  (Right p) <- getInput
  assertEqual "" 563 $ part1' p

testPart2 :: Assertion
testPart2 = do
  (Right p) <- getInput2
  assertEqual "" (629,629,4,2,0,0) $ part2' p

tests :: [TestTree]
tests = [
        testCase "part1" testPart1,
        testCase "part2" testPart2
        ]
