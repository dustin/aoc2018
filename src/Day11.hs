{-# LANGUAGE OverloadedStrings #-}

module Day11 where

powerLevel :: Int -> Int -> Int -> Int
powerLevel sn r c = let rid = r + 10
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
  let mapped = map (\l@(h:_) -> (sum $ (uncurry (powerLevel sn)) <$> l, h)) cells
  print $ maximum mapped

grouping' :: Int -> (Int,Int) -> [(Int,Int)]
grouping' sz (x,y) = [(x',y') | x' <- [x.. x + sz], y' <- [y .. y+sz]]

part2 :: IO ()
part2 = do
  let sn = 7139
  let cells = [grouping' sz (x,y) | sz <- [1..35], -- 35 is an assumption
                x <- [1.. 300],
                y <- [1.. 300],
                x + sz <= 300,
                y + sz <= 300 ]
  let mapped = map (\l@(h:_) -> (sum $! (uncurry (powerLevel sn)) <$> l, h, sqrt $ fromIntegral $ length l)) cells
  print $ maximum mapped
