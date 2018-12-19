{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import qualified Data.Array.Unboxed as A
import           Data.Ix            (Ix)
import           Data.List          (intercalate)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (mapMaybe)
import           Debug.Trace        (trace)

type Thing = Char

open, trees, lumberyard :: Thing
open = '.'
trees = '|'
lumberyard = '#'

type World = A.UArray (Int,Int) Thing

bounds :: World -> (Int,Int)
bounds w = snd $ A.bounds w

adjacent :: World -> (Int,Int) -> [(Int,Int)]
adjacent w (x,y) = [(x + n, y + m) | n <- [-1..1], m <- [-1..1],
                    not (n == 0 && m == 0)]
  where (mx,my) = bounds w

adjacent' :: World -> (Int,Int) -> [Thing]
adjacent' w p = mapMaybe lu $ adjacent w p
  where
    (mxx, mxy) = bounds w
    lu p'@(x,y)
      | x < 0 || y < 0 || x > mxx || y > mxy = Nothing
      | otherwise = Just (w A.! p')

draw :: World -> String
draw w = intercalate "\n" $ map row [0..my]

  where
    (mx,my) = bounds w
    row y = map (\x -> w A.! (x,y)) [0..mx]

parseInput :: [String] -> World
parseInput lns = A.array ((0,0),(mx,my)) els
                 where els = concatMap (\(y,r) -> map (\(x,c) -> ((x,y),c)) $ zip [0..] r) $ zip [0..] lns
                       mx = maximum (fst . fst <$> els)
                       my = maximum (snd . fst <$> els)

getInput :: IO World
getInput = parseInput . lines <$> readFile "input/day18"

tx :: Int -> World -> World
tx n = head . drop n . iterate tx1

tx1 :: World -> World
tx1 w = mapa transform w

  where
    mapa :: (Ix k, A.IArray u a, A.IArray u' b) => (k -> a -> b) -> u k a -> u' k b
    mapa f a = A.listArray (A.bounds a) (uncurry f <$> A.assocs a)

    transform :: (Int,Int) -> Thing -> Thing
    transform p '.' | atLeast p 3 trees = trees
    transform p '|' | atLeast p 3 lumberyard = lumberyard
    transform p '#' | not (atLeast p 1 trees && atLeast p 1 lumberyard) = open
    transform _ x = x

    atLeast :: (Int,Int) -> Int -> Thing -> Bool
    atLeast p = go (adjacent' w p)
      where
        go _ 0 _ = True
        go [] _ _ = False
        go (x:xs) n t = go xs (if t == x then n - 1 else n) t

ofType :: World -> Thing -> Int
ofType w t = length $ filter (== t) (A.elems w)

-- 467819
part1 :: IO ()
part1 = print =<< score . tx 10 <$> getInput

-- Get the position of the start of the first cycle and the cycle length from a list
findCycle :: Ord b => (a -> b) -> [a] -> (Int,Int)
findCycle f = go 0 mempty
  where go n mem (x:xs) = case Map.lookup t mem of
                            Nothing -> go (n+1) (Map.insert t n mem) xs
                            Just o  -> (o,n - o)
          where t = f x

score :: World -> Int
score w = ofType w trees * ofType w lumberyard

-- 195305

part2' :: World -> Int
part2' w = let allstates = iterate tx1 w
               (f,cl) = findCycle id allstates
               rep = drop f allstates
               off = (1000000000 - f) `mod` cl
               w' = head . drop off $ rep in
             score w'

part2 :: IO ()
part2 = print =<< part2' <$> getInput
