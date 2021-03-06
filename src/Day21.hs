{-# LANGUAGE OverloadedStrings #-}

module Day21 where

import qualified Data.Set   as Set

import           Advent.AoC (parseFile)
import           Elfcode

getInput :: IO Program
getInput = parseFile parseProg "input/day21"

findR5s :: Program -> [Int]
findR5s p = map (\(_,rs) -> reg rs 5) $ iterate (uncurry (execUntil p (\(ip,_) -> ip == 28))) (0,(0,0,0,0,0,0))

-- 3941014
part1 :: IO ()
part1 = do
  p <- getInput
  print $ (head . filter (/= 0) . findR5s) p


part2' :: Program -> Int
part2' = go mempty 0 . findR5s
  where
    go seen prev (x:xs)
      | Set.member x seen = prev
      | otherwise = go (Set.insert x seen) x xs

part2 :: IO ()
part2 = print =<< part2' <$> getInput
