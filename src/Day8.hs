{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Debug.Trace (trace)
import Data.List (mapAccumL)
import qualified Data.Tree as Tree

readInput :: IO (Tree.Tree [Int])
readInput =  readTree <$> map read <$> words <$> readFile "input/day8"

-- 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
-- A----------------------------------
--     B----------- C-----------
--                      D-----


-- [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]
-- A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
-- B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
-- C, which has 1 child node (D) and 1 metadata entry (2).
-- D, which has 0 child nodes and 1 metadata entry (99).

readTree :: [Int] -> Tree.Tree [Int]
readTree l@(c:m:xs) =
  let (xs', subs) = mapAccumL (\xs' ts ->
                                  let t = readTree xs' in
                                    (drop (sz t) xs', t)) xs [1..c] in
    Tree.Node (take m xs') subs

  where
    sz :: Tree.Tree [Int] -> Int
    sz (Tree.Node a bs) = foldr (\x o -> o + sz x) (2 + length a) bs

part1 :: IO ()
part1 = print =<< (sum .concat . Tree.flatten) <$> readInput

count :: Tree.Tree [Int] -> Int
count (Tree.Node xs []) = sum xs
count (Tree.Node xs subs) = foldr (\x o -> o + count (subs `at` x)) 0 xs

  where at :: Tree.Forest [Int] -> Int -> Tree.Tree [Int]
        at l i
          | i > length l = Tree.Node [] []
          | otherwise = l !! (i-1)

part2 :: IO ()
part2 = print =<< count <$> readInput

