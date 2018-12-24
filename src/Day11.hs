{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import qualified Data.Array.Unboxed          as A
import           Debug.Trace                 (trace)

import           Control.Parallel.Strategies (parList, parMap, rdeepseq, using)

powerLevel :: Int -> (Int, Int) -> Int
powerLevel sn (r,c) = let rid = r + 10
                          st = rid * c
                          cell = st + sn
                          pl = (rid * cell) `div` 100 `mod` 10 in
                        pl - 5
part1 :: IO ()
part1 = do
  let sn = 7139
  let cells = [grouping 3 (x,y) | x <- [1.. 300 -2], y <- [1.. 300 -2]]
  let mapped = map (\l@(h:_) -> (sum $ powerLevel sn <$> l, h)) cells
  print $ maximum mapped

grouping :: Int -> (Int,Int) -> [(Int,Int)]
grouping sz (x,y) = [(x',y') | x' <- [x.. x + sz-1], y' <- [y .. y+sz-1]]

largestAtSize :: Int -> Int -> (Int, (Int,Int))
largestAtSize sn sz =
  let cells = [grouping sz (x,y) |
                x <- [1.. 300],
                y <- [1.. 300],
                x + sz - 1 <= 300,
                y + sz - 1 <= 300 ] in
  maximum $ map (\l@(h:_) -> (sum $! powerLevel sn <$> l, h)) cells

newtype Grid = Grid (A.Array (Int,Int) Int) deriving (Show)

gl :: Grid -> (Int,Int) -> Int
gl (Grid g') = let bs = A.bounds g' in checked bs

  where checked ((xmn,ymn),(xmx,ymx)) (x,y) =
          if y < ymn || y > ymx || x < xmn || x > xmx then 0 else g' A.! (x,y)

mkGrid :: Int -> Int -> Int -> Grid
mkGrid sn xs ys = Grid $ A.array ((1,1),(xs,ys))
                  [((x,y), powerLevel sn (x,y)) | x <- [1..xs], y <- [1..ys]]

part2 :: IO ()
part2 = do
  let sn = 7139
  print $ maximum (map (\sz -> (largestAtSize sn sz, sz)) [1..18] `using` parList rdeepseq)

-- Using an array instead.
part2b :: IO ()
part2b = print $ maximum (map (\sz -> (largestAtSize' sz , sz)) [1..18] `using` parList rdeepseq)

  where
    g = gl $ mkGrid 7139 300 300

    largestAtSize' :: Int -> (Int, (Int,Int))
    largestAtSize' sz =
      let cells = [grouping sz (x,y) |
                   x <- [1.. 300],
                   y <- [1.. 300],
                   x + sz - 1 <= 300,
                   y + sz - 1 <= 300 ] in
        maximum $ map (\l@(h:_) -> (sum $! g <$> l, h)) cells

-- https://en.wikipedia.org/wiki/Summed-area_table
mkSumAreaTable :: Grid -> ((Int,Int) -> Int)
mkSumAreaTable g@(Grid g') = gl (Grid mkArry)

  where
        mkArry :: A.Array (Int,Int) Int
        mkArry = let b@((xmin,ymin),(xmax,ymax)) = A.bounds g'
                     a = A.array b [((x,y), gl g (x,y)
                                            + subSum a (x, y-1)
                                            + subSum a (x-1, y)
                                            - subSum a (x-1, y-1))
                                   | x <- [xmin..xmax], y <- [ymin..ymax]] in a

        subSum :: A.Array (Int,Int) Int -> (Int,Int) -> Int
        subSum _ (0,_) = 0
        subSum _ (_,0) = 0
        subSum a (x,y) = a A.! (x,y)

sumIn :: ((Int,Int) -> Int) -> (Int,Int) -> (Int,Int) -> Int
sumIn g (lx,ly) (hx,hy) = let d = g (hx, hy)
                              b = g (hx, ly-1)
                              c = g (lx-1, hy)
                              a = g (lx-1, ly-1) in
                            d - b - c + a

-- Using a sum area table (229,61,16)
part2c :: IO ()
part2c = print $ maxSeeded 7139

maxSeeded :: Int -> (Int,(Int,Int),Int)
maxSeeded sn = maximum $ parMap rdeepseq (\sz -> let (sm,pos) = largestAtSize' sz in (sm, pos, sz)) [1..300]

  where g = mkSumAreaTable (mkGrid sn 300 300)

        suma :: Int -> (Int,Int) -> Int
        suma sz (x,y) = sumIn g (x,y) (x+sz-1,y+sz-1)

        largestAtSize' :: Int -> (Int, (Int,Int))
        largestAtSize' sz =
          let cells = [(x,y) |
                       x <- [1.. 301 - sz],
                       y <- [1.. 301 - sz]] in
            maximum $ map (\l -> (suma sz l, l)) cells


allBounds :: IO ()
allBounds = do
  let alls = parMap rdeepseq largestI [0..8]
  putStrLn $ "lowest: " <> show (minimum alls)
  putStrLn $ "highest: " <> show (maximum alls)

  where
    largestI :: Int -> (Int,Int,(Int,Int))
    largestI i = let (_,at,sz) =  maxSeeded i in
                   trace (show (sz,i,at)) (sz,i,at)
