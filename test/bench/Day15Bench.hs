module Day15Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day15

instance NFData World where
  rnf w@(World m) = w `seq` m `seq` ()

getInp :: String -> IO (World)
getInp s = parseInput . lines <$> readFile s

tests :: [Benchmark]
tests = [
  env (getInp "input/day15.sample3") $ \ ~w -> bgroup "elfbattle" [
      bench "part2" $ nf (\w' -> fst $ play w' 0 (mkHit 3)) w
      ]
  ]
