module Day10Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day10

instance NFData Vec where
  rnf v@(Vec (x1,y1) (x2,y2)) = v `seq` x1 `seq` x2 `seq` y1 `seq` y2 `seq` ()

tests :: [Benchmark]
tests = [
  env getInput $ \ ~vs -> bgroup "starpattern" [
      bench "bounds" $ nf bounds vs,
      bench "part2" $ nf part2' vs
      ]
  ]
