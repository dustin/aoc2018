{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import qualified Data.IntMap   as IntMap
import           Data.List     (foldl')
import qualified Data.Sequence as Seq

insertClockwise :: [Int] -> Int -> [Int]
insertClockwise xs m = m : rot2 xs

  where rot2 = rot . rot
        rot [x]    = [x]
        rot (x:xs) = xs <> [x]

play :: Int -> Int -> Int
play p m = let players = [1..p]
               marbles = [1..m]
               plays = zip (cycle players) marbles
               (scores, _) = foldl' addOne (mempty, [0]) plays
               total = sum <$> scores in
             maximum total

  where
    addOne (scores, circle) (player, marble)
      | marble `mod` 23 == 0 = let (l,(t:r)) = splitAt 6 (reverse circle) in
                                 (IntMap.insertWith (<>) player [t,marble] scores,
                                  reverse (r<>l))
      | otherwise = (scores, insertClockwise circle marble)


part1 :: IO ()
part1 = do
  print $ play 428 72061

-- Seq is a lot less dumb than lists for this problem.  Did it for part 2

insertClockwise' :: Seq.Seq Int -> Int -> Seq.Seq Int
insertClockwise' xs m = Seq.insertAt 0 m (rot2 xs)

  where
    rot2 :: Seq.Seq Int -> Seq.Seq Int
    rot2 s = let s' = Seq.take 2 s in
               (Seq.drop (length s') s <> s')

play' :: Int -> Int -> Int
play' p m = let players = [1..p]
                marbles = [1..m]
                plays = zip (cycle players) marbles
                (scores, _) = foldl' addOne (mempty, Seq.singleton 0) plays
                total = sum <$> scores in
              maximum total

  where
    addOne :: (IntMap.IntMap [Int], Seq.Seq Int) -> (Int, Int) -> (IntMap.IntMap [Int], Seq.Seq Int)
    addOne (scores, circle) (player, marble)
      | marble `mod` 23 == 0 = let (l,t Seq.:<| r) = Seq.splitAt (Seq.length circle - 7) circle in
                                 (IntMap.insertWith (<>) player [t,marble] scores,
                                   (1 `seq` r<>l))
      | otherwise = (scores, insertClockwise' circle marble)


part2 :: IO ()
part2 = do
  print $ play' 428 7206100
