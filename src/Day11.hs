{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Control.Monad (mapM_)
import Control.Parallel.Strategies (parList, using, rdeepseq)

powerLevel :: Int -> (Int, Int) -> Int
powerLevel sn (r,c) = let rid = r + 10
                          st = rid * c
                          cell = st + sn
                          pl = (rid * cell) `div` 100 `mod` 10 in
                        pl - 5

grouping :: (Int,Int) -> [(Int,Int)]
grouping (x,y) = [(x',y') | x' <- [x.. x + 2], y' <- [y .. y+2]]

part1 :: IO ()
part1 = do
  let sn = 7139
  let cells = [grouping (x,y) | x <- [1.. 300 -2], y <- [1.. 300 -2]]
  let mapped = map (\l@(h:_) -> (sum $ powerLevel sn <$> l, h)) cells
  print $ maximum mapped

findMax :: Ord b => (a -> b) -> [a] -> a
findMax f (x:xs) = go xs x
  where go [] r = r
        go (x':xs') r
          | f x' < f r = r
          | otherwise = go xs' x'

grouping' :: Int -> (Int,Int) -> [(Int,Int)]
grouping' sz (x,y) = [(x',y') | x' <- [x.. x + sz-1], y' <- [y .. y+sz-1]]

largestAtSize :: Int -> Int -> (Int, (Int,Int))
largestAtSize sn sz =
  let cells = [grouping' sz (x,y) |
                x <- [1.. 300],
                y <- [1.. 300],
                x + sz - 1 <= 300,
                y + sz - 1 <= 300 ] in
  maximum $ map (\l@(h:_) -> (sum $! powerLevel sn <$> l, h)) cells

part2 :: IO ()
part2 = do
  let sn = 7139
  print $ maximum (map (\sz -> (largestAtSize sn sz, sz)) [1..300] `using` parList rdeepseq)
