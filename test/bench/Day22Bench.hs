{-# LANGUAGE OverloadedStrings #-}

module Day22Bench where

import Criterion (Benchmark, bench, bgroup, env, nf, whnf, nfIO)
import Criterion.Main (defaultMain)
import System.IO (IOMode(..), Handle, withFile)
import Control.DeepSeq (NFData(..))

import Day22

instance NFData Survey where
  rnf (Survey a) = a `seq` a `seq` ()

instance NFData Tool where
  rnf t = t `seq` t `seq` ()

testS :: IO Survey
testS = pure $ survey (10,10,510) (13,13)

tests :: [Benchmark]
tests = [
  env testS $ \ ~s -> bgroup "elfcave" [
      bench "findPath" $ nf (\s' -> mapCosts s' ((0,0),Torch) ((10,10),Torch)) s
      ]
  ]
