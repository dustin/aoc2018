{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import           Advent.AoC      (mdist2)
import qualified Data.Map.Strict as Map
import           Data.Ord        (comparing)
import           Data.Semigroup  ((<>))

-- Create a grid of every point within the given min (x,y) and max (x,y)
grid :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
grid (lx,ly) (hx,hy) = [(x,y) | x <- [lx..hx], y <- [ly..hy]]

-- Create a grid of everything withing the bounds of the given points
grid' :: [(Int,Int)] -> [(Int,Int)]
grid' = uncurry grid . bounds

-- list of points to the lowest x,y and the highest x,y
bounds :: [(Int,Int)] -> ((Int,Int), (Int,Int))
bounds points = let xs = fst <$> points
                    ys = snd <$> points in
                  ((minimum xs, minimum ys), (maximum xs, maximum ys))

readInput :: IO [(Int,Int)]
readInput = do
  datas <- lines <$> readFile "input/day6"
  pure $ map (\[a,b] -> (a,b)) $ map (map read . words . filter (/= ',')) datas

-- 4143
part1 :: IO ()
part1 = do
  pairs <- readInput

  -- Shortest distances to all points within the grid, including duplicates.
  let m = Map.fromListWith (\a b -> case comparing minimum a b of
                                      EQ -> a <> b
                                      LT -> a
                                      GT -> b) [(pos, [(mdist2 pos op, op)]) | pos <- grid' pairs, op <- pairs]

  -- Transform the above to only single point values with duplicates omitted.
  let mm = head <$> Map.filter ((== 1) . length) m

  -- Map of input points to the number of points to which they were the shortest.
  let md = Map.fromListWith (+) $ map (\(_,(_,op)) -> (op,1)) $ Map.toList mm

  print $ maximum md

-- 35039
part2 :: IO ()
part2 = do
  pairs <- readInput

  -- This one is super easy, just count the sum of distances for every
  -- point in the grid.
  let distances = map (\c -> foldr (\x o -> o + mdist2 x c) 0 pairs) $ grid' pairs

  -- And then throw away any that are too far.
  print $ length $ filter (< 10000) distances
