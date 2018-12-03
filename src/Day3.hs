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

part1 :: IO ()
part1 = do
  lns <- map pack <$> lines <$> readFile "input/day3"
  let (Right ls) = traverse (A.parseOnly aclaim) lns
  print $ length $ Map.filter (>1) $ Map.fromListWith (+) $ map (\(x,y) -> ((x,y),1)) $ expand ls

part2 :: IO ()
part2 = do
  lns <- map pack <$> lines <$> readFile "input/day3"
  let (Right ls) = traverse (A.parseOnly aclaim) lns
  let counts = Map.fromListWith (+) $ map (\(x,y) -> ((x,y),1)) $ expand ls
  let a = foldr (\c@(Claim i _ _) o -> let segs = expand [c] in
                                 if total counts segs == length segs then c
                                 else o) undefined ls
  print a

    where
      total :: Map.Map (Int,Int) Int -> [(Int,Int)] -> Int
      total m = foldr (\x o -> o + Map.findWithDefault 0 x m) 0
