{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Debug.Trace (trace)
import Data.Ix (Ix)
import qualified Data.Array.Unboxed as A

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

newtype Grid = Grid (A.Array Int (A.Array Int Int)) deriving (Show)

gl :: Grid -> (Int,Int) -> Int
gl g@(Grid g') (x,y) = let ((omn,ox),(imn,ix)) = gbounds g in
  if y < omn || y > ox || x < imn || x > ix then 0 else g' A.! x A.! y

gbounds :: Grid -> ((Int,Int), (Int,Int))
gbounds (Grid g) = let o@(l,_) = A.bounds g
                       i = A.bounds (g A.! l) in (o,i)

mkGrid :: Int -> Int -> Int -> Grid
mkGrid sn xs ys = Grid $ A.array (1,ys) $ map (\y -> (y, A.array (1,xs) $
                                                        map (\x -> (x, powerLevel sn (x,y))) [1..xs]))
                  [1..ys]

part2 :: IO ()
part2 = do
  let sn = 7139
  print $ maximum (map (\sz -> (largestAtSize sn sz, sz)) [1..18] `using` parList rdeepseq)

-- Using an array instead.
part2b :: IO ()
part2b = do
  print $ maximum (map (\sz -> (largestAtSize' sz , sz)) [1..18] `using` parList rdeepseq)

  where
    g = gl $ mkGrid 7139 300 300

    largestAtSize' :: Int -> (Int, (Int,Int))
    largestAtSize' sz =
      let cells = [grouping' sz (x,y) |
                   x <- [1.. 300],
                   y <- [1.. 300],
                   x + sz - 1 <= 300,
                   y + sz - 1 <= 300 ] in
        maximum $ map (\l@(h:_) -> (sum $! g <$> l, h)) cells

-- https://en.wikipedia.org/wiki/Summed-area_table
mkSumAreaTable :: Grid -> ((Int,Int) -> Int)
mkSumAreaTable g = gl (Grid mkArry)

  where
        mkArry :: A.Array Int (A.Array Int Int)
        mkArry = let (ob@(omin,omax),ib@(imin,imax)) = gbounds g
                     a = A.array ob $
                         map (\y -> (y, A.array ib $
                                        map (\x ->
                                                (x, gl g (x,y)
                                                    + subSum a (x,   y-1)
                                                    + subSum a (x-1, y)
                                                    - subSum a (x-1, y-1))) [imin..imax]))
                         [omin..omax] in a

        subSum :: (A.Array Int (A.Array Int Int)) -> (Int,Int) -> Int
        subSum _ (0,_) = 0
        subSum _ (_,0) = 0
        subSum a (x,y) = a A.! y A.! x

sumIn :: ((Int,Int) -> Int) -> (Int,Int) -> (Int,Int) -> Int
sumIn g (lx,ly) (hx,hy) = let d = g (hx, hy)
                              b = g (hx, ly-1)
                              c = g (lx-1, hy)
                              a = g (lx-1, ly-1) in
                            d - b - c + a

-- Using a sum area table (229,61,16)
part2c :: IO ()
part2c = do
  print $ maximum $ parMap rdeepseq (\sz -> (largestAtSize' sz, sz)) [1..300]

  where g = mkSumAreaTable (mkGrid 7139 300 300)

        suma :: Int -> (Int,Int) -> Int
        suma sz (x,y) = sumIn g (x,y) (x+sz-1,y+sz-1)

        largestAtSize' :: Int -> (Int, (Int,Int))
        largestAtSize' sz =
          let cells = [(x,y) |
                       x <- [1.. 301 - sz],
                       y <- [1.. 301 - sz]] in
            maximum $ map (\l -> (suma sz l, l)) cells
