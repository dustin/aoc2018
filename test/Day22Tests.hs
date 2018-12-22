{-# LANGUAGE OverloadedStrings #-}

module Day22Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day22

import qualified Data.Array          as A

gel :: Survey -> (Int,Int) -> Int
gel (Survey a) p = snd $ a A.! p

ggi :: Survey -> (Int,Int) -> Int
ggi (Survey a) p = fst $ a A.! p

testExample :: Assertion
testExample = do
  let s@(Survey a) = survey (10,10,510)
  assertEqual "0,0 el" 510 (gel s (0,0))
  assertEqual "0,0 t" Rocky $ cellType s (0,0)

  assertEqual "1,0 el" 17317 (gel s (1,0))
  assertEqual "1,0 t" Wet $ cellType s (1,0)

  assertEqual "0,1 el" 8415 (gel s (0,1))
  assertEqual "0,1 t" Rocky $ cellType s (0,1)

  assertEqual "1,1 el" 1805 (gel s (1,1))
  assertEqual "1,1 t" Narrow $ cellType s (1,1)

  assertEqual "10,10 el" 510 (gel s (10,10))
  assertEqual "10,10 t" Rocky $ cellType s (10,10)

  assertEqual "risk" 114 (riskLevel s)

testPart1 :: Assertion
testPart1 = do
  assertEqual "" 7299 $ riskLevel myCave

testPart2 :: Assertion
testPart2 = do
  pure ()


tests :: [TestTree]
tests = [
  testCase "example" testExample,
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
