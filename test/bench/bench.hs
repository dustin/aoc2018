module Main (main) where

import Criterion (bench, bgroup, env, nf, whnf, nfIO)
import Criterion.Main (defaultMain)
import System.IO (IOMode(..), Handle, withFile)

import Day18Bench
import Day20Bench
import Day21Bench
import Day22Bench

main :: IO ()
main = defaultMain [
  bgroup "day18" Day18Bench.tests,
  bgroup "day20" Day20Bench.tests,
  bgroup "day21" Day21Bench.tests,
  bgroup "day22" Day22Bench.tests
  ]
