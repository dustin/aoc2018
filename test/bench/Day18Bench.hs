{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Day18Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day18

instance NFData World where
  rnf !w = w `seq` ()

tests :: [Benchmark]
tests = [
  env getInput $ \ ~w -> bgroup "worldly" [
      bench "transform" $ nf tx1 w,
      bench "100th" $ nf (tx 100) w,
      bench "scoring" $ nf score w,
      bench "part2" $ nf part2' w
      ]
  ]
