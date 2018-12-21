{-# LANGUAGE OverloadedStrings #-}

module Day19 where

import qualified Data.Attoparsec.Text as A
import           Data.List            (union)
import           Data.Text            (pack)

import           Elfcode

--
-- Stuff for part1.
--

getInput :: IO (Either String Program)
getInput = A.parseOnly parseProg . pack <$> readFile "input/day19"

-- (2520,865,1,865,256,864)
part1 :: IO ()
part1 = getInput >>= \(Right prog) -> print $ execute prog 0 (0,0,0,0,0,0)

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
