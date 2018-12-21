{-# LANGUAGE OverloadedStrings #-}

module Day21Bench where

import Criterion (Benchmark, bench, bgroup, env, nf, whnf, nfIO)
import Criterion.Main (defaultMain)
import System.IO (IOMode(..), Handle, withFile)
import Control.DeepSeq (NFData(..))

import Day21

inp :: IO Program
inp = getInput >>= \(Right prog) -> pure prog

tests :: [Benchmark]
tests = [
  env inp $ \ ~p -> bgroup "elfcode" [
      bench "part1" $ nf (head . drop 1 . findR5s) p,
      bench "100000 ticks" $ nf (\p' -> execute' p' 0 (0,0,0,0,0,0) 100000) p,
      bench "until 7130602" $ whnf (\p' -> dropWhile (/= 7130602) $ findR5s p') p
      ]
  ]
