module Main (main) where

import           Criterion      (bench, bgroup, env, nf, nfIO, whnf)
import           Criterion.Main (defaultMain)
import           System.IO      (Handle, IOMode (..), withFile)

import           Day15Bench
import           Day18Bench
import           Day20Bench
import           Day21Bench
import           Day22Bench

main :: IO ()
main = defaultMain [
  bgroup "day15" Day15Bench.tests,
  bgroup "day18" Day18Bench.tests,
  bgroup "day20" Day20Bench.tests,
  bgroup "day21" Day21Bench.tests,
  bgroup "day22" Day22Bench.tests
  ]
