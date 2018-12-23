{-# LANGUAGE OverloadedStrings #-}

module Day22Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day22

import qualified Data.Array            as A
import qualified Data.Map.Strict       as Map

gel :: Survey -> (Int,Int) -> Int
gel (Survey a) p = a A.! p

testExample :: Assertion
testExample = do
  let s@(Survey a) = survey (10,10,510) (10,10)
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

testExampleP2 :: Assertion
testExampleP2 = do
  let s = survey (10,10,510) (20,20)
      Just (c,_) = dijkstra (neighbors s) ((0,0),Torch) ((10,10),Torch)

  assertEqual "" 45 c

testPart1 :: Assertion
testPart1 = do
  assertEqual "" 7299 $ riskLevel myCave

testPart2 :: Assertion
testPart2 = do
  assertEqual "" 1008 part2'


tests :: [TestTree]
tests = [
  testCase "example" testExample,
  testCase "example part 2" testExampleP2,
  testCase "part1" testPart1,
  testCase "part2" testPart2
  ]
