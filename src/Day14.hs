{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Debug.Trace (trace)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Char (digitToInt)
import qualified Data.Sequence as Seq

wtelf :: Int -> Int -> Int -> [Int]
wtelf a b n = go (Seq.fromList [a,b]) (0,1)

  where
    go :: Seq.Seq Int -> (Int,Int) -> [Int]
    go s (e1,e2)
      | length s > n+10 = toList . Seq.take 10 . Seq.drop n $ s
      | otherwise = let s1 = (Seq.index s e1)
                        s2 = (Seq.index s e2)
                        (d1,d2) = (s1 + s2) `divMod` 10
                        toAdd = Seq.fromList $ if d1 == 0 then [d2] else [d1, d2]
                        s' = s <> toAdd
                        e1' = (e1 + 1 + s1) `mod` Seq.length s'
                        e2' = (e2 + 1 + s2) `mod` Seq.length s' in
                        go s' (e1', e2')

-- 8176111038
part1 :: IO ()
part1 = putStrLn (concatMap show $ wtelf 3 7 890691)

wtelf' :: Int -> Int -> [Int] -> Int
wtelf' a b m = go (Seq.fromList [a,b]) (0,1)

  where
    go :: Seq.Seq Int -> (Int,Int) -> Int
    go s (e1,e2)
      | match s > 0 = match s
      | otherwise = let s1 = (Seq.index s e1)
                        s2 = (Seq.index s e2)
                        (d1,d2) = (s1 + s2) `divMod` 10
                        toAdd = Seq.fromList $ if d1 == 0 then [d2] else [d1, d2]
                        s' = s <> toAdd
                        e1' = (e1 + 1 + s1) `mod` Seq.length s'
                        e2' = (e2 + 1 + s2) `mod` Seq.length s' in
                        go s' (e1', e2')

    match :: Seq.Seq Int -> Int
    match s = let base = Seq.length s - (length m + 2)
                  (_,end) = Seq.splitAt base s in
                at base (toList end)
    at :: Int -> [Int] -> Int
    at _ [] = -1
    at b s = if isPrefixOf m s
             then b
             else at (b+1) (tail s)

-- 20225578
part2 :: IO ()
part2 = print $ wtelf' 3 7 $ map digitToInt "890691"
