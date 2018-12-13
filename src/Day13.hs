{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Debug.Trace (trace)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Control.Monad (replicateM, mapM_)
import Control.Applicative ((<|>))
import Data.List (sort, foldl')
import Data.Maybe (isJust, fromJust)

data Dir = N | E | S | W deriving (Bounded, Enum, Show, Eq)

move :: Dir -> (Int,Int) -> (Int,Int)
move N (x,y) = (x,y-1)
move S (x,y) = (x,y+1)
move E (x,y) = (x+1,y)
move W (x,y) = (x-1,y)

turn :: NextTurn -> Dir -> Dir
turn Straight = id
turn TurnLeft = pred'
turn TurnRight = succ'

data NextTurn = TurnLeft | Straight | TurnRight deriving (Show, Bounded, Enum, Eq)

-- A circular succ
succ' :: (Bounded a, Enum a, Eq a) => a -> a
succ' a
  | a == maxBound = minBound
  | otherwise = succ a

-- A circular pred
pred' :: (Bounded a, Enum a, Eq a) => a -> a
pred' a
  | a == minBound = maxBound
  | otherwise = pred a

data Cart = Cart (Int,Int) Dir NextTurn deriving(Show)

instance Eq Cart where
  (Cart a _ _) == (Cart b _ _) = a == b

instance Ord Cart where
  (Cart (ax,ay) _ _) <= (Cart (bx,by) _ _) = (ay,ax) <= (by,bx)

data Segment = UpDown | LeftRight | Intersection | Curve Char | Empty deriving (Eq)

instance Show Segment where
  show UpDown = "'"
  show LeftRight = "-"
  show Intersection = "+"
  show (Curve c) = [c]
  show _ = " "

data World = World (Map.Map (Int,Int) Segment) [Cart]

parseInput :: [String] -> World
parseInput lns =
  World (Map.fromList $
          concatMap (\(y,r) -> map (\(x,s) -> ((x,y),seg s)) $ zip [0..] r) $ zip [0..] lns)
  (sort $ concatMap (\(y,r) -> foldr (\(x,c) o -> maybe o (:o) (cart c (x,y))) [] $ zip [0..] r) $ zip [0..] lns)

  where
    seg '|' = UpDown
    seg 'v' = UpDown
    seg '^' = UpDown
    seg '-' = LeftRight
    seg '>' = LeftRight
    seg '<' = LeftRight
    seg '+' = Intersection
    seg '/' = Curve '/'
    seg '\\' = Curve '\\'
    seg _ = Empty

    cart :: Char -> (Int,Int) -> Maybe Cart
    cart c pos = Cart pos <$> dir c <*> Just TurnLeft

    dir '>' = Just E
    dir '^' = Just N
    dir 'v' = Just S
    dir '<' = Just W
    dir _ = Nothing

getInput :: IO World
getInput = parseInput . lines <$> readFile "input/day13"

moveCarts :: World -> Either (Int,Int) World
moveCarts w@(World m carts) =
  World m <$> moveCarts' (sort carts)

  where
    moveCarts' :: [Cart] -> Either (Int,Int) [Cart]
    moveCarts' cs = go cs []
      where
        go [] r = Right r
        go (c:xs) r = let c' = moveCart c in
                        if pos c `elem` (map pos r <> map pos xs) then
                          Left (pos c)
                        else go xs (c':r)

    moveCart :: Cart -> Cart
    moveCart c@(Cart pos dir nextTurn) =
      let nextPos = move dir pos in
        case m Map.! nextPos of
                  Intersection -> Cart nextPos (turn nextTurn dir) (succ' nextTurn)
                  UpDown -> Cart nextPos dir nextTurn
                  LeftRight -> Cart nextPos dir nextTurn
                  Curve c -> Cart nextPos (curve c dir) nextTurn

    curve :: Char -> Dir -> Dir
    curve '/' N = E
    curve '/' E = N
    curve '/' W = S
    curve '/' S = W
    curve '\\' N = W
    curve '\\' E = S
    curve '\\' W = N
    curve '\\' S = E

    pos :: Cart -> (Int,Int)
    pos (Cart p _ _) = p

findCrash :: World -> (Int,Int)
findCrash w = case moveCarts w of
                Left x -> x
                Right w' -> findCrash w'

part1 :: IO ()
part1 = do
  w@(World _ carts) <- getInput
  print carts
  print $ findCrash w
