{-# LANGUAGE OverloadedStrings #-}

module Day20Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Control.Applicative   (liftA2)
import           Data.List             (sort)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Text.Megaparsec       (parse)

import           Data.Text             (Text, unpack)

import           Day20

testParser :: [TestTree]
testParser =
  map (\(t, want) -> testCase (unpack t) $ assertEqual "" (TheMap <$> want) (parse parseInput "" t)) [
  ("^NE$", Right [Dirs [N,E]]),
  ("^NE(S|W)$", Right [Dirs [N,E],
                       Sub [[Dirs [S]],
                            [Dirs [W]]]]),
  ("^NE(S|W(N|S))$", Right [Dirs [N,E],
                            Sub [[Dirs [S]],
                                 [Dirs [W],
                                  Sub [[Dirs [N]],
                                       [Dirs [S]]]]]]),
  ("^NE(S|)E$", Right [Dirs [N,E],
                       Sub [[Dirs [S]]],
                       Dirs [E]]),
  ("^NE(S|W|)E$", Right [Dirs [N,E],
                         Sub [[Dirs [S]], [Dirs [W]]],
                         Dirs [E]])
  ]

testMostDoors :: [TestTree]
testMostDoors =
  map (\(t, want) -> testCase (unpack t) $ assertEqual "" want (mostDoors <$> parse parseInput "" t)) [
  ("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", Right 23),
  ("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", Right 31)
  ]

testConnections :: [TestTree]
testConnections =
  map (\(t, want) -> testCase (unpack t) $ assertEqual "" (Right $ Map.fromList (sort want)) (fmap sort <$> (connections <$> parse parseInput "" t))) [
  ("^NN$", [((0,0), [(0,-1)]),
            ((0,-1), [(0,-2)])]),
  ("^WNE$", [((0,0), [(-1,0)]),
             ((-1,0), [(-1,-1)]),
             ((-1,-1), [(0,-1)])]),
  ("^N(E|W)$", [((0,0), [(0,-1)]),
                ((0,-1), [(-1,-1), (1,-1)])]),
  ("^N(E|W|)N$", [((0,0), [(0,-1)]),
                  ((0,-1), [(-1,-1), (0, -2), (1,-1)])])

  ]

testPart1 :: Assertion
testPart1 = do
  (Right p) <- getInput
  assertEqual "" 3872 $ mostDoors p

testPart2 :: Assertion
testPart2 = do
  (Right p) <- getInput
  assertEqual "" 8600 $ part2' p


tests :: [TestTree]
tests = [
        testGroup "parser" testParser,
        testGroup "most doors" testMostDoors,
        testGroup "connections" testConnections,

        testCase "part1" testPart1,
        testCase "part2" testPart2
        ]
