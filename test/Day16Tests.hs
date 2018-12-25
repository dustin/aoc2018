{-# LANGUAGE OverloadedStrings #-}

module Day16Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day16

testPart1 :: Assertion
testPart1 = assertEqual "" 563 =<< part1' <$> getInput

testPart2 :: Assertion
testPart2 = assertEqual "" (629,629,4,2,0,0) =<< part2' <$> getInput2

tests :: [TestTree]
tests = [
        testCase "part1" testPart1,
        testCase "part2" testPart2
        ]
