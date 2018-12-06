{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import Data.Semigroup ((<>))
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

mdist :: (Int,Int) -> (Int,Int) -> Int
mdist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

part1 :: IO ()
part1 = do
  datas <- lines <$> readFile "input/day6"
  let pairs = map (\[a,b] -> (a,b)) $ map (map read . words . filter (/= ',')) datas :: [(Int,Int)]
  print pairs

  let lbounds@(lx,ly) = (minimum $ fst <$> pairs, minimum $ snd <$> pairs)
  let hbounds@(hx,hy) = (maximum $ fst <$> pairs, maximum $ snd <$> pairs)

  let grid = [(x,y) | x <- [lx..hx], y <- [ly..hy]]

  let m = Map.fromListWith (\a b -> case comparing minimum a b of
                                      EQ -> a <> b
                                      LT -> a
                                      GT -> b) [(pos, [(mdist pos op, op)]) | pos <- grid, op <- pairs]
  let mm = head <$> Map.filter ((== 1) . length) m

  let inner = Map.filter (\(_,(x,y)) -> x > lx && x < hx && y > ly && y < hy) mm

  let md = Map.fromListWith (+) $ map (\(_,(d,op)) -> (op,1)) $ Map.toList inner

  print (lbounds, hbounds)
  -- print mm
  -- print inner
  print md

  print $ maximum md

part2 :: IO ()
part2 = do
  datas <- lines <$> readFile "input/day6"
  let pairs = map (\[a,b] -> (a,b)) $ map (map read . words . filter (/= ',')) datas :: [(Int,Int)]

  let lbounds@(lx,ly) = (minimum $ fst <$> pairs, minimum $ snd <$> pairs)
  let hbounds@(hx,hy) = (maximum $ fst <$> pairs, maximum $ snd <$> pairs)

  let grid = [(x,y) | x <- [lx..hx], y <- [ly..hy]]

  let distances = map (\c -> foldr (\x o -> o + mdist x c) 0 pairs) grid

  print $ length $ filter (< 10000) distances
