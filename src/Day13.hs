{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Debug.Trace (trace)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Control.Monad (replicateM, mapM_)
import Control.Applicative ((<|>))
import Data.List (sort, foldl', intercalate)
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

instance Show World where
  show (World m carts) = let m' = Map.union (Map.fromList $ map (\(Cart p d _) -> (p,s d)) carts) (show <$> m)
                             mx = maximum $ (fst <$> Map.keys m')
                             my = maximum $ (snd <$> Map.keys m') in
                           intercalate "\n" (map (\y -> concatMap (\x -> m' Map.! (x,y)) [0..mx]) [0..my])
    where s N = "^"
          s E = ">"
          s W = "<"
          s S = "v"

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


moveCart :: World -> Cart -> Cart
moveCart (World m _)  c@(Cart pos dir nextTurn) =
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

moveCarts :: World -> Either (Int,Int) World
moveCarts w@(World m carts) =
  World m <$> moveCarts' (sort carts)

  where
    moveCarts' :: [Cart] -> Either (Int,Int) [Cart]
    moveCarts' cs = go cs []
      where
        go [] r = Right r
        go (c:xs) r = let c' = moveCart w c in
                        if c `elem` (r <> xs) then
                          Left (pos c)
                        else go xs (c':r)

findCrash :: World -> (Int,Int)
findCrash w = case moveCarts w of
                Left x -> x
                Right w' -> findCrash w'

-- (102,114)
part1 :: IO ()
part1 = do
  w <- getInput
  print $ findCrash w

moveCarts2 :: World -> World
moveCarts2 w@(World m carts) =
  World m $ moveCarts' (sort carts)

  where
    moveCarts' :: [Cart] -> [Cart]
    moveCarts' cs = go cs []
      where
        go [] r = r
        go (c:xs) r = let c' = moveCart w c in
                        if c' `elem` (r <> xs) then
                          go (filter (/= c') xs) (filter (/= c') r)
                        else go xs (c':r)

reduceCarts :: World -> (Int,Int)
reduceCarts w = let w'@(World _ c) = moveCarts2 w in
                  case c of
                    [Cart p _ _] -> p
                    otherwise -> reduceCarts w'

-- (146,87)
part2 :: IO ()
part2 = do
  w <- getInput
  print $ reduceCarts w
