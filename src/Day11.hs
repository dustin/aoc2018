{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Debug.Trace (trace)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA

import Control.Monad (mapM_)
import Control.Parallel.Strategies (NFData, parMap, parList, using, rdeepseq)

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

-- newtype Grid = Grid (A.Array Int (UA.UArray Int Int)) deriving (Show)
type Grid = ((Int,Int) -> Int)

mkGrid :: Int -> Int -> Int -> Grid
mkGrid sn xs ys = let a = mkArry in \(x,y) -> a A.! x UA.! y

  where
    mkArry :: UA.Array Int (UA.Array Int Int)
    mkArry = UA.array (1,xs) $ map (\x -> (x, UA.array (1,ys) $
                                              map (\y -> (y, powerLevel sn (x,y))) [1..ys]))
             [1..xs]

part2 :: IO ()
part2 = do
  let sn = 7139
  print $ maximum (map (\sz -> (largestAtSize sn sz, sz)) [1..18] `using` parList rdeepseq)

-- Using an array instead.
part2b :: IO ()
part2b = do
  print $ maximum (map (\sz -> (largestAtSize' sz , sz)) [1..18] `using` parList rdeepseq)

  where
    g = mkGrid 7139 300 300

    largestAtSize' :: Int -> (Int, (Int,Int))
    largestAtSize' sz =
      let cells = [grouping' sz (x,y) |
                   x <- [1.. 300],
                   y <- [1.. 300],
                   x + sz - 1 <= 300,
                   y + sz - 1 <= 300 ] in
        maximum $ map (\l@(h:_) -> (sum $! g <$> l, h)) cells

-- https://en.wikipedia.org/wiki/Summed-area_table
mkSumAreaTable :: Int -> Grid
mkSumAreaTable sn = let a = mkArry in \(x,y) -> a UA.! x UA.! y

  where g = mkGrid sn 300 300
        mkArry :: UA.Array Int (UA.Array Int Int)
        mkArry = let a = UA.array (1,300) $
                         mp (\x -> (x, UA.array (1,300) $
                                        mp (\y ->
                                               (y, g (x,y)
                                                   + subSum a (x,y-1)
                                                   + subSum a (x-1, y)
                                                   - subSum a (x-1, y-1))) [1..300]))
                         [1..300] in a

        mp :: NFData b => (a -> b) -> [a] -> [b]
        mp = parMap rdeepseq

        subSum :: (UA.Array Int (UA.Array Int Int)) -> (Int,Int) -> Int
        subSum _ (0,_) = 0
        subSum _ (_,0) = 0
        subSum a (x,y) = a UA.! x UA.! y

-- Using a sum area table
part2c :: IO ()
part2c = do
  print $ maximum $ parMap rdeepseq (\sz -> (largestAtSize' sz, sz)) [1..300]

  where g = mkSumAreaTable 7139

        suma :: Int -> (Int,Int) -> Int
        suma sz (x,y) = g (x+sz-1, y+sz-1) - g (x+sz-1, y) - g (x, y+sz-1) + g (x,y)

        largestAtSize' :: Int -> (Int, (Int,Int))
        largestAtSize' sz =
          let cells = [(x,y) |
                       x <- [1.. 301 - sz],
                       y <- [1.. 301 - sz]] in
            maximum $ map (\l -> (suma sz l, l)) cells
