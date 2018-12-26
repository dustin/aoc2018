{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import qualified Data.Array.Unboxed as A
import           Data.Ix            (Ix)
import           Data.List          (intercalate)

import Search (findCycle)

type Thing = Char

open, trees, lumberyard :: Thing
open = '.'
trees = '|'
lumberyard = '#'

type World = A.UArray (Int,Int) Thing

bounds :: World -> (Int,Int)
bounds w = snd $ A.bounds w

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = x `seq` [
  (x-1, y+1), (x,y+1), (x+1, y+1),
  (x-1, y),            (x+1, y),
  (x-1, y-1), (x,y-1), (x+1, y-1)
  ]

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
    transform _ x   = x

    is p t = (A.inRange . A.bounds) w p && t == w A.! p

    atLeast :: (Int,Int) -> Int -> Thing -> Bool
    atLeast p = go (adjacent p)
      where
        go _ 0 _      = True
        go [] _ _     = False
        go (x:xs) n t = go xs (if is x t then n - 1 else n) t

ofType :: World -> Thing -> Int
ofType w t = length $ filter (== t) (A.elems w)

-- 467819
part1 :: IO ()
part1 = print =<< score . tx 10 <$> getInput

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
