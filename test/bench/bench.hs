module Main (main) where

import Criterion (bench, bgroup, env, nf, whnf, nfIO)
import Criterion.Main (defaultMain)
import System.IO (IOMode(..), Handle, withFile)

import Day18Bench

main :: IO ()
main = defaultMain [ bgroup "day18" Day18Bench.tests ]
