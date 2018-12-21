{-# LANGUAGE OverloadedStrings #-}

module Day20Bench where

import Criterion (Benchmark, bench, bgroup, env, nf, whnf, nfIO)
import Criterion.Main (defaultMain)
import System.IO (IOMode(..), Handle, withFile)
import Control.DeepSeq (NFData(..))

import Data.Text (pack)
import Text.Megaparsec (parse)

import Day20

tests :: [Benchmark]
tests = [
  bench "small" $ nf (parse parseInput "") "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$",
  env (pack <$> readFile "input/day20") $ \ ~t -> bgroup "parsing" [
      bench "parse" $ nf (parse parseInput "") t
      ]
  ]
