{-# LANGUAGE OverloadedStrings #-}

module Day22Bench where

import           Control.DeepSeq (NFData (..))
import           Criterion       (Benchmark, bench, bgroup, env, nf)

import           Day22
import           Search          (dijkstra)

instance NFData Survey where
  rnf (Survey a) = a `seq` a `seq` ()

instance NFData Tool where
  rnf t = t `seq` t `seq` ()

testS :: IO Survey
testS = pure $ survey (10,10,510) (13,13)

tests :: [Benchmark]
tests = [
  env testS $ \ ~s -> bgroup "elfcave" [
      bench "findPath" $ nf (\s' -> let Just (_,p) = dijkstra (neighbors s') ((0,0),Torch) ((10,10),Torch) in p) s
      ]
  ]
