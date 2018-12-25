{-# LANGUAGE OverloadedStrings #-}

module Day21Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           AoC                   (parseFile)
import           Day21
import           Elfcode

getOrig :: IO Program
getOrig = parseFile parseProg "input/day21.orig"

testInputIntegrity :: Assertion
testInputIntegrity = do
  o <- getOrig
  n <- getInput
  assertEqual "" o n

testPart1 :: Assertion
testPart1 = do
  p <- getInput
  assertEqual "" 3941014 $ head . filter (/= 0) . findR5s $ p

testPart2 :: Assertion
testPart2 = assertEqual "" 13775890 =<< part2' <$> getInput


tests :: [TestTree]
tests = [
  testCase "input integrity" testInputIntegrity,
  testCase "part1" testPart1
  -- ,testCase "part2" testPart2 -- too slow
  ]
