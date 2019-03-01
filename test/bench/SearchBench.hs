{-# LANGUAGE OverloadedStrings #-}

module SearchBench where

import           Criterion (Benchmark, bench, whnf)

import           Search

tests :: [Benchmark]
tests = [
  bench "0 37 100000" $ whnf (binSearch (flip compare 37) 0) 100000,
  bench "0 3700 100000" $ whnf (binSearch (flip compare 3700) 0) 100000
  ]
