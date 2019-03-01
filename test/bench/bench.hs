module Main (main) where

import           Criterion      (bgroup)
import           Criterion.Main (defaultMain)

import           AoCBench
import           Day10Bench
import           Day15Bench
import           Day18Bench
import           Day20Bench
import           Day21Bench
import           Day22Bench
import           SearchBench

main :: IO ()
main = defaultMain [
  bgroup "aoc" AoCBench.tests,
  bgroup "search" SearchBench.tests,
  bgroup "day10" Day10Bench.tests,
  bgroup "day15" Day15Bench.tests,
  bgroup "day18" Day18Bench.tests,
  bgroup "day20" Day20Bench.tests,
  bgroup "day21" Day21Bench.tests,
  bgroup "day22" Day22Bench.tests
  ]
