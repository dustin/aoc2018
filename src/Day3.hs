{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Control.Monad (guard)
import qualified Data.Map.Strict as Map

import qualified Data.Text.Encoding as E
import qualified Data.Attoparsec.Text as A
import Data.Text (Text, pack, unpack)

data Claim = Claim Int (Int,Int) (Int,Int) deriving (Show)

aclaim :: A.Parser Claim
aclaim = do
  i <- "#" *> A.decimal <* " @ "
  x <- A.decimal <* ","
  y <- A.decimal <* ": "
  w <- A.decimal <* "x"
  h <- A.decimal

  pure $ Claim i (x,y) (w,h)

expand :: [Claim] -> [(Int,Int)]
expand = concatMap (\(Claim _ (x,y) (w,h)) -> [(x',y') | x' <- [x .. x+w-1], y' <-[y .. y+h-1]])

counts :: [Claim] -> Map.Map (Int,Int) Int
counts = Map.fromListWith (+) . map (\(x,y) -> ((x,y),1)) . expand

parseClaims :: [String] -> Either String [Claim]
parseClaims = traverse (A.parseOnly aclaim . pack)

part1 :: IO ()
part1 = do
  (Right ls) <- parseClaims <$> lines <$> readFile "input/day3"
  print $ length $ Map.filter (>1) $ counts ls

part2 :: IO ()
part2 = do
  (Right ls) <- parseClaims <$> lines <$> readFile "input/day3"
  let cs = counts ls
  let a = filter (\c@(Claim i _ _) -> let segs = expand [c] in total cs segs == length segs) ls
  print $ head a

    where
      total :: Map.Map (Int,Int) Int -> [(Int,Int)] -> Int
      total m = foldr (\x o -> o + Map.findWithDefault 0 x m) 0
