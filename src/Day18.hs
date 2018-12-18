{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day18 where

import Debug.Trace (trace)
import           Data.List            (intercalate)
import qualified Data.Map.Strict      as Map
import Data.Maybe (catMaybes)

data Thing = Open | Trees | Lumberyard deriving (Eq)

instance Show Thing where
  show Open = "."
  show Trees = "|"
  show Lumberyard = "#"

newtype World = World (Map.Map (Int,Int) Thing) deriving (Eq)

bounds :: World -> (Int,Int)
bounds (World m) = (maximum (fst <$> ks), maximum (snd <$> ks))

  where ks = Map.keys m

adjacent :: World -> (Int,Int) -> [(Int,Int)]
adjacent w (x,y) = [(x + n, y + m) | n <- [-1..1], m <- [-1..1],
                    not (n == 0 && m == 0)]
  where (mx,my) = bounds w

adjacent' :: World -> (Int,Int) -> [Thing]
adjacent' w@(World m) p = catMaybes $ map (`Map.lookup` m) $ adjacent w p

instance Show World where
  show w@(World m) = intercalate "\n" $ map row [0..my]

    where
      (mx,my) = bounds w
      row y = concatMap (\x -> show $ m Map.! (x,y)) [0..mx]

parseInput :: [String] -> World
parseInput lns = World $ Map.fromList $ concatMap (\(y,r) -> map (\(x,c) -> ((x,y),p c)) $ zip [0..] r) $ zip [0..] lns

  where p :: Char -> Thing
        p '.' = Open
        p '|' = Trees
        p '#' = Lumberyard
        p x = error ("can't parse " <> show x)

getInput :: IO (World)
getInput = parseInput . lines <$> readFile "input/day18"

tx :: Int -> World -> World
tx 0 w = w
tx n w@(World m) = tx (n-1) $ World $ Map.mapWithKey (transform w) m

tx1 :: World -> World
tx1 w@(World m) = World $ Map.mapWithKey (transform w) m

transform :: World -> (Int,Int) -> Thing -> Thing
transform w p Open = if count w p Trees >= 3 then Trees else Open
transform w p Trees = if count w p Lumberyard >= 3 then Lumberyard else Trees
transform w p Lumberyard = if (count w p Trees > 0 && count w p Lumberyard > 0) then Lumberyard else Open

count w p t = length . filter (== t) $ adjacent' w p

ofType :: World -> Thing -> Int
ofType (World m) t = length $ Map.filter (== t) m

-- 467819
part1 :: IO ()
part1 = do
  w <- getInput
  print w
  print "..."
  let w' = tx 10 w
  print $ (ofType w' Trees) * (ofType w' Lumberyard)

findCycle :: World -> (Int,Int)
findCycle w = (go 0 mempty w)
  where
    go :: Int -> Map.Map String Int -> World -> (Int,Int)
    go n mem w'@(World m) = case Map.lookup s mem of
                   Nothing -> go (n+1) (Map.insert s n mem) w''
                   Just x -> (x,n)
      where w'' = World $ Map.mapWithKey (transform w') m
            s = show w''

score :: World -> Int
score w = (ofType w Trees) * (ofType w Lumberyard)

-- 195305
part2 :: IO ()
part2 = do
  w <- getInput
  let (f,t) = findCycle w
  let (f,t) = (550,578)
  let allstates = iterate tx1 w
  let rep = drop f allstates
  let off = (1000000000 - f) `mod` (t - f)
  let w' = head . drop off $ rep
  print $ score w'
