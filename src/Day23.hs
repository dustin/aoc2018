{-# LANGUAGE OverloadedStrings #-}

module Day23 where

import qualified Data.Attoparsec.Text as A
import           Data.Ord             (comparing)
import Data.Foldable (maximumBy)
import Data.Text (pack)

type Pos = (Int,Int,Int)

data Nanobot = Nanobot !Pos !Int deriving (Show)

parseBot :: A.Parser Nanobot
parseBot = do
  x <- "pos=<" *> num <* ","
  y <- num <* ","
  z <- num <* ">, r="
  r <- num

  pure $ Nanobot (x,y,z) r

    where num  = A.signed A.decimal

getInput :: IO (Either String [Nanobot])
getInput = A.parseOnly (parseBot `A.sepBy` "\n") . pack <$> readFile "input/day23"

-- should be the square root of the squares of the differences on each axis.
distance :: (Int,Int,Int) -> (Int,Int,Int) -> Int
distance (x1,y1,z1) (x2,y2,z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

botDistance :: Nanobot -> Nanobot -> Int
botDistance (Nanobot p1 _) (Nanobot p2 _) = distance p1 p2

inRange :: Nanobot -> Nanobot -> Bool
inRange a@(Nanobot _ r) b = botDistance a b <= r

strongest :: [Nanobot] -> Nanobot
strongest = maximumBy (comparing (\(Nanobot _ r) -> r))

part1' :: [Nanobot] -> Int
part1' bots = length $ filter (inRange (strongest bots)) bots

part1 :: IO ()
part1 = do
  (Right bots) <- getInput
  print $ part1' bots
