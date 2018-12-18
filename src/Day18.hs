{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Debug.Trace     (trace)

type Thing = Char

open, trees, lumberyard :: Thing
open = '.'
trees = '|'
lumberyard = '#'

newtype World = World (Map.Map (Int,Int) Thing) deriving (Eq, Ord)

bounds :: World -> (Int,Int)
bounds (World m) = (maximum (fst <$> ks), maximum (snd <$> ks))

  where ks = Map.keys m

adjacent :: World -> (Int,Int) -> [(Int,Int)]
adjacent w (x,y) = [(x + n, y + m) | n <- [-1..1], m <- [-1..1],
                    not (n == 0 && m == 0)]
  where (mx,my) = bounds w

adjacent' :: World -> (Int,Int) -> [Thing]
adjacent' w@(World m) p = mapMaybe (`Map.lookup` m) $ adjacent w p

instance Show World where
  show w@(World m) = intercalate "\n" $ map row [0..my]

    where
      (mx,my) = bounds w
      row y = concatMap (\x -> show $ m Map.! (x,y)) [0..mx]

parseInput :: [String] -> World
parseInput lns = World $ Map.fromList $ concatMap (\(y,r) -> map (\(x,c) -> ((x,y),c)) $ zip [0..] r) $ zip [0..] lns

getInput :: IO World
getInput = parseInput . lines <$> readFile "input/day18"

tx :: Int -> World -> World
tx n = head . drop n . iterate tx1

tx1 :: World -> World
tx1 w@(World m) = World $ Map.mapWithKey transform m

  where
    transform :: (Int,Int) -> Thing -> Thing
    transform p '.' = if atLeast p 3 trees then trees else open
    transform p '|' = if atLeast p 3 lumberyard then lumberyard else trees
    transform p '#' = if atLeast p 1 trees && atLeast p 1 lumberyard then lumberyard else open

    atLeast :: (Int,Int) -> Int -> Thing -> Bool
    atLeast p = go (adjacent' w p)
      where
        go _ 0 _ = True
        go [] _ _ = False
        go (x:xs) n t
          | t == x = go xs (n-1) t
          | otherwise = go xs n t

ofType :: World -> Thing -> Int
ofType (World m) t = length $ Map.filter (== t) m

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
