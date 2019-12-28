{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import qualified Data.Map.Strict            as Map

import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC                 (Parser, parseFile)

data Claim = Claim Int (Int,Int) (Int,Int) deriving (Show)

aclaim :: Parser Claim
aclaim = do
  i <- "#" *> decimal <* " @ "
  x <- decimal <* ","
  y <- decimal <* ": "
  w <- decimal <* "x"
  h <- decimal

  pure $ Claim i (x,y) (w,h)

expand :: [Claim] -> [(Int,Int)]
expand = concatMap (\(Claim _ (x,y) (w,h)) -> [(x',y') | x' <- [x .. x+w-1], y' <-[y .. y+h-1]])

counts :: [Claim] -> Map.Map (Int,Int) Int
counts = Map.fromListWith (+) . map (\(x,y) -> ((x,y),1)) . expand

getInput :: IO [Claim]
getInput = parseFile (aclaim `endBy` "\n") "input/day3"

-- 118858
part1 :: IO ()
part1 = do
  ls <- getInput
  print $ length $ Map.filter (>1) $ counts ls

-- Claim 1100 (355,404) (29,11)
part2 :: IO ()
part2 = do
  ls <- getInput
  let cs = counts ls
  let a = filter (\c@(Claim _ _ _) -> let segs = expand [c] in total cs segs == length segs) ls
  print $ head a

    where
      total :: Map.Map (Int,Int) Int -> [(Int,Int)] -> Int
      total m = foldr (\x o -> o + Map.findWithDefault 0 x m) 0
