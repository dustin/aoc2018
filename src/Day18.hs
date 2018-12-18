{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Debug.Trace     (trace)

data Thing = Open | Trees | Lumberyard deriving (Eq, Ord)

instance Show Thing where
  show Open       = "."
  show Trees      = "|"
  show Lumberyard = "#"

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
parseInput lns = World $ Map.fromList $ concatMap (\(y,r) -> map (\(x,c) -> ((x,y),p c)) $ zip [0..] r) $ zip [0..] lns

  where p :: Char -> Thing
        p '.' = Open
        p '|' = Trees
        p '#' = Lumberyard
        p x   = error ("can't parse " <> show x)

getInput :: IO World
getInput = parseInput . lines <$> readFile "input/day18"

tx :: Int -> World -> World
tx n = head . drop n . iterate tx1

tx1 :: World -> World
tx1 w@(World m) = World $ Map.mapWithKey transform m

  where
    transform :: (Int,Int) -> Thing -> Thing
    transform p Open = if count p Trees >= 3 then Trees else Open
    transform p Trees = if count p Lumberyard >= 3 then Lumberyard else Trees
    transform p Lumberyard = if count p Trees > 0 && count p Lumberyard > 0 then Lumberyard else Open

    count p t = length . filter (== t) $ adjacent' w p

ofType :: World -> Thing -> Int
ofType (World m) t = length $ Map.filter (== t) m

-- 467819
part1 :: IO ()
part1 = do
  w <- getInput
  print w
  print $ score (tx 10 w)

-- Get the position of the start of the first cycle and the cycle length from a list
findCycle :: Ord b => (a -> b) -> [a] -> (Int,Int)
findCycle f = go 0 mempty
  where go n mem (x:xs) = case Map.lookup t mem of
                            Nothing -> go (n+1) (Map.insert t n mem) xs
                            Just o  -> (o,n - o)
          where t = f x

score :: World -> Int
score w = ofType w Trees * ofType w Lumberyard

-- 195305
part2 :: IO ()
part2 = do
  w <- getInput
  let allstates = iterate tx1 w
  let (f,cl) = findCycle id allstates
  let rep = drop f allstates
  let off = (1000000000 - f) `mod` cl
  let w' = head . drop off $ rep
  print $ score w'
