module Day18Bench where

import Criterion (Benchmark, bench, bgroup, env, nf, whnf, nfIO)
import Criterion.Main (defaultMain)
import System.IO (IOMode(..), Handle, withFile)
import Control.DeepSeq (NFData(..))

import Day18

instance NFData World where
  rnf w@(World m) = w `seq` m `seq` ()

tests :: [Benchmark]
tests = [
  env getInput $ \ ~w -> bgroup "worldly" [
      bench "transform" $ nf tx1 w,
      bench "100th" $ nf (tx 100) w
      ]
  ]
