{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import           Data.List  (union)

import           Advent.AoC (parseFile)

import           Elfcode

--
-- Stuff for part1.
--

getInput :: IO Program
getInput = parseFile parseProg "input/day19"

-- (2520,865,1,865,256,864)
part1 :: IO ()
part1 = getInput >>= \ prog -> print $ execute prog 0 (0,0,0,0,0,0)

-- 27941760
part2 :: IO ()
part2 = print $ sum (divisors 10551264)

divisors :: (Integral a, Eq a) => a -> [a]
divisors 1 = [1]
divisors 2 = [1]
divisors n = let lower = [x | x <- [1..isqrt n], n `mod` x == 0] in
               lower `union` (map (div n) lower)

  where
    isqrt :: Integral a => a -> a
    isqrt = ceiling . sqrt . fromIntegral
