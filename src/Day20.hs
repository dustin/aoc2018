{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Day20 where

import           Control.Applicative  (some, (<|>))
import           Control.DeepSeq      (NFData (..))
import           Data.Foldable        (foldl')
import           Data.List            (union)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text, pack)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)


import           Text.Megaparsec      (ParseError, Parsec, between, parse,
                                       sepBy)
import           Text.Megaparsec.Char (char)

data Directions = Dirs [Dir] | Sub [[Directions]] deriving (Generic, Eq, Show)

newtype TheMap = TheMap [Directions] deriving (Generic, Eq, Show)

data Dir = N | E | S | W deriving (Generic, Show, Eq, Enum, Bounded)

instance NFData Dir
instance NFData Directions
instance NFData TheMap

type Parser = Parsec Void Text

parseInput :: Parser TheMap
parseInput = TheMap <$> ("^" *> subExpr <* "$")
  where

    subExpr :: Parser [Directions]
    subExpr = some (Dirs <$> some dir <|> subDirs)

    dir :: Parser Dir
    dir = N <$ "N"
          <|> E <$ "E"
          <|> S <$ "S"
          <|> W <$ "W"

    subDirs :: Parser Directions
    subDirs = do
      stuff <- between "(" ")" ((subExpr <|> ([] <$ "")) `sepBy` (char '|'))
      pure $ Sub $ filter (not . null) stuff

type XY = (Int,Int)

move :: XY -> Dir -> XY
move (x,y) N = (x,y-1)
move (x,y) S = (x,y+1)
move (x,y) E = (x+1,y)
move (x,y) W = (x-1,y)

-- Build a map of every point and where you can get to from it.
connections :: TheMap -> Map XY [XY]
connections (TheMap ps) = go ps (0,0) mempty
  where
    go :: [Directions] -> (Int,Int) -> Map XY [XY] -> Map XY [XY]
    go [] _ r = r

    go (Dirs dirs:xs) p m = let (np, conns) = foldl' (\(p',o) d ->
                                                        let p'' = move p' d in (p'', (p',[p'']):o)) (p,[]) dirs
                                m' = Map.unionWith (union) m $ Map.fromList conns in
                              go xs np m'

    go (Sub ds:xs) p m = go xs p $ foldr (\d m' -> go d p m') m ds

reachable :: TheMap -> Map XY Int
reachable tm = go mempty mempty 0 [(0,0)]

  where
    conns = connections tm

    go :: Set XY -> Map XY Int -> Int -> [XY] -> Map XY Int
    go _ r _ [] = r
    go seen r d (x:xs)
      | Set.member x seen    = go seen r d xs
      | r Map.!? x > Just d  = go seen r d xs
      | otherwise            = let s' = Set.insert x seen
                                   lm = Map.insert x d r
                                   rm = go s' lm d xs in
                                 go s' rm (d+1) (Map.findWithDefault [] x conns)

mostDoors :: TheMap -> Int
mostDoors = maximum . reachable

getInput :: IO (Either (ParseError Char Void) TheMap)
getInput = (parse parseInput "") . pack <$> readFile "input/day20"

-- 3872
part1 :: IO ()
part1 = getInput >>= \(Right x) -> print $ mostDoors x

part2' :: TheMap -> Int
part2' = length . Map.filter (>= 1000) . reachable

part2 :: IO ()
part2 = getInput >>= \(Right x) -> print $ part2' x
