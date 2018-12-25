{-# LANGUAGE OverloadedStrings #-}

module AoCBench where

import           Criterion (Benchmark, bench, whnf)

import           AoC

tests :: [Benchmark]
tests = [
  bench "mdist2" $ whnf (mdist2 (3,4)) (5,6),
  bench "mdist3" $ whnf (mdist3 (3,4,5)) (5,6,7),
  bench "mdist4" $ whnf (mdist4 (3,4,5,6)) (5,6,7,8)
  ]
