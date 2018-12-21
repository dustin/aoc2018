{-# LANGUAGE OverloadedStrings #-}

module Day21Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Data.Attoparsec.Text  as A
import           Data.Text             (pack)

import           Day21
import           Elfcode

getOrig :: IO (Either String Program)
getOrig = A.parseOnly parseProg . pack <$> readFile "input/day21.orig"

testInputIntegrity :: Assertion
testInputIntegrity = do
  o <- getOrig
  n <- getInput
  assertEqual "" o n

testPart1 :: Assertion
testPart1 = do
  (Right p) <- getInput
  assertEqual "" 3941014 $ head . filter (/= 0) . findR5s $ p

testPart2 :: Assertion
testPart2 = do
  (Right p) <- getInput
  assertEqual "" 13775890 $ part2' p


tests :: [TestTree]
tests = [
  testCase "input integrity" testInputIntegrity,
  testCase "part1" testPart1
  -- ,testCase "part2" testPart2 -- too slow
  ]
