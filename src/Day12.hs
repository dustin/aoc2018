{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Debug.Trace (trace)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Control.Monad (replicateM, mapM_)
import Control.Applicative ((<|>))
import Data.Array.Unboxed as Ar
import Data.List (foldl')

{-
initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
-}

data Pot = Empty | Plant deriving (Bounded, Enum, Eq)

data Pots = Pots (Ar.UArray Int Bool)

fromBool :: Bool -> Pot
fromBool True = Plant
fromBool _ = Empty

toBool :: Pot -> Bool
toBool Plant = True
toBool _ = False

instance Show Pots where
  show (Pots ps) = show (Ar.bounds ps) <> " => " <> concatMap (show.fromBool) (Ar.elems ps)

instance Show Pot where
  show Plant = "#"
  show Empty = "."

data Input = Input Pots [Transformer]

instance Show Input where
  show (Input ps ts) = show ps <> " " <> show ts

data Transformer = Transformer [Pot] Pot

instance Show Transformer where
  show (Transformer pots dest) = concatMap show pots <> " → " <> show dest

parsePot :: A.Parser Pot
parsePot = A.char '.' *> pure Empty <|>
           A.char '#' *> pure Plant

parseInput :: A.Parser Input
parseInput = do
  istate <- "initial state: " *> A.many1 parsePot
  _ <- A.many' A.space

  transes <- A.many1 trans

  pure $ Input (Pots $ Ar.array (0, length istate) $ zip [0..] (map toBool istate)) transes

  where
    trans :: A.Parser Transformer
    trans = do
      pots <- replicateM 5 parsePot
      tn <- " => " *> parsePot <* A.many' A.space

      pure $ Transformer pots tn

getInput :: IO Input
getInput = do
  (Right (Input i ts)) <- A.parseOnly parseInput . pack <$> readFile "input/day12"
  pure $ Input i (filter (\(Transformer _ dst) -> dst == Plant) ts)

stateSum :: Pots -> Int
stateSum (Pots a) = sum $ fst <$> (filter snd $ Ar.assocs a)

applyAll :: Pots -> [Transformer] -> Pots
applyAll (Pots ps) ts = let nl = foldr step [] [l-2..h+2]
                            nb = foldl' (\(l',h') x -> (min l' (fst x), max h' (fst x))) (l+2,h-2) nl in
                          Pots $ Ar.array nb nl

  where
    (l,h) = Ar.bounds ps
    step :: Int -> [(Int,Bool)] -> [(Int,Bool)]
    step x o = if go seg ts == Plant then (x, True):o else o

      where seg = [at n | n <- [x-2 .. x+2]]
            at :: Int -> Pot
            at n = if n < l || n > h then Empty else fromBool (ps Ar.! n)

            go :: [Pot] -> [Transformer] -> Pot
            go _ [] = Empty
            go ps (t@(Transformer tmatch dst):ts)
              | ps == tmatch = dst
              | otherwise = go ps ts

-- expect 2140
part1 :: IO ()
part1 = do
  (Input istate ts) <- getInput

  let ns = foldl' (\o _ -> applyAll o ts) istate [1..20]

  print $ stateSum ns

part2 :: IO ()
part2 = do
  (Input istate ts) <- getInput

  let ns = foldl' (\o _ -> applyAll o ts) istate [1..50000000000]

  print $ stateSum ns
