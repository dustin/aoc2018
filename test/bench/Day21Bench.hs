{-# LANGUAGE OverloadedStrings #-}

module Day21Bench where

import           Criterion (Benchmark, bench, bgroup, env, nf, whnf)

import           Day21
import           Elfcode

tests :: [Benchmark]
tests = [
  env getInput $ \ ~p -> bgroup "elfcode" [
      bench "part1" $ nf (head . drop 1 . findR5s) p,
      bench "100000 ticks" $ nf (\p' -> execute' p' 0 (0,0,0,0,0,0) 100000) p,
      bench "until 7130602" $ whnf (\p' -> dropWhile (/= 7130602) $ findR5s p') p
      ]
  ]
