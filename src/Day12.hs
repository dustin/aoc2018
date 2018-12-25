{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import           Control.Applicative  ((<|>))
import           Control.Monad        (replicateM)
import           Data.Array.Unboxed   as Ar
import           Data.List            (foldl')
import           Data.Maybe           (fromMaybe)
import           Text.Megaparsec      (some)
import           Text.Megaparsec.Char (char, space)

import           AoC                  (Parser, parseFile)

type Pot = Bool

newtype Pots = Pots (Ar.UArray Int Bool)

instance Show Pots where
  show (Pots ps) = show (Ar.bounds ps) <> " => " <> map (\x -> if x then '#' else '.') (Ar.elems ps)

data Input = Input Pots [Transformer]

instance Show Input where
  show (Input ps ts) = show ps <> " " <> show ts

type Transformer = ([Pot], Pot)

parsePot :: Parser Pot
parsePot = False <$ char '.' <|> True <$ char '#'

parseInput :: Parser Input
parseInput = do
  istate <- "initial state: " *> some parsePot <* space

  transes <- some trans

  pure $ Input (Pots $ Ar.array (0, length istate) $ zip [0..] istate) transes

  where
    trans :: Parser Transformer
    trans = do
      pots <- replicateM 5 parsePot
      tn <- " => " *> parsePot <* space

      pure (pots, tn)

getInput :: IO Input
getInput = parseFile parseInput "input/day12"

stateSum :: Pots -> Int
stateSum (Pots a) = sum $ fst <$> filter snd (Ar.assocs a)

applyAll :: Pots -> [Transformer] -> Pots
applyAll (Pots ps) ts = let nl = foldr step [] [l-2..h+2]
                            nb = foldl' (\(l',h') x -> (min l' (fst x), max h' (fst x))) (l+2,h-2) nl in
                          Pots $ Ar.array nb nl

  where
    (l,h) = Ar.bounds ps
    step :: Int -> [(Int,Bool)] -> [(Int,Bool)]
    step x o = if fromMaybe False $ lookup seg ts then (x, True):o else o

      where seg = [at n | n <- [x-2 .. x+2]]
            at :: Int -> Pot
            at n = if n < l || n > h then False else ps Ar.! n

-- expect 2140
part1 :: IO ()
part1 = do
  (Input istate ts) <- getInput

  let ns = foldl' (\o _ -> applyAll o ts) istate [1..20]

  print $ stateSum ns

-- expect 1900000000384
part2 :: IO ()
part2 = do
  (Input istate ts) <- getInput

  -- Stabilizes after 100 and then the whole thing just shifts one to
  -- the right.  Can compute the answer pretty easily.
  let (Pots ps) = foldl' (\o _ -> applyAll o ts) istate [1..100]
  let ns = (map (toEnum.fst) $ filter snd (Ar.assocs ps)) :: [Integer]
  print $ sum (map (+ (50000000000-100)) ns)
