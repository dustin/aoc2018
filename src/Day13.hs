{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Data.List (sort, intercalate)
import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as Map

data Dir = N | E | S | W deriving (Bounded, Enum, Show, Eq)

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

data Cart = Cart (Int,Int) Dir NextTurn

instance Show Cart where
  show (Cart _ N _) = "^"
  show (Cart _ S _) = "v"
  show (Cart _ E _) = ">"
  show (Cart _ W _) = "<"

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
  show (World m carts) = let m' = Map.union (Map.fromList $ map (\c@(Cart p _ _) -> (p,r c)) carts) (show <$> m)
                             mx = maximum (fst <$> Map.keys m')
                             my = maximum (snd <$> Map.keys m') in
                           intercalate "\n" (map (\y -> concatMap (\x -> m' Map.! (x,y)) [0..mx]) [0..my])

    where
      r :: Show a => a -> String
      r a = "\ESC[41;1m" <> show a <> "\ESC[0m"

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

  where
    move :: Dir -> (Int,Int) -> (Int,Int)
    move N (x,y) = (x,y-1)
    move S (x,y) = (x,y+1)
    move E (x,y) = (x+1,y)
    move W (x,y) = (x-1,y)

    curve :: Char -> Dir -> Dir
    curve '/' N = E
    curve '/' E = N
    curve '/' W = S
    curve '/' S = W
    curve '\\' N = W
    curve '\\' E = S
    curve '\\' W = N
    curve '\\' S = E

    turn :: NextTurn -> Dir -> Dir
    turn Straight = id
    turn TurnLeft = pred'
    turn TurnRight = succ'

findFail :: (Either a b -> Either a b) -> Either a b -> a
findFail f = either id (findFail f . f . Right)

-- (102,114)
part1 :: IO ()
part1 = do
  w <- getInput
  print $ findFail (moveCarts =<<) (Right w)

  where
    moveCarts :: World -> Either (Int,Int) World
    moveCarts w@(World m carts) =
      World m <$> go (sort carts) []

      where
        go :: [Cart] -> [Cart] -> Either (Int,Int) [Cart]
        go [] r = Right r
        go (c:xs) r = let c'@(Cart pos _ _) = moveCart w c in
                        if c' `elem` (r <> xs) then Left pos
                        else go xs (c':r)

-- (146,87)
part2 :: IO ()
part2 = do
  w <- getInput
  print $ findFail (moveCarts =<<) (Right w)

  where
    moveCarts :: World -> Either (Int,Int) World
    moveCarts w@(World m carts) = World m <$> go (sort carts) []

      where
        go :: [Cart] -> [Cart] -> Either (Int,Int) [Cart]
        go [c] [] = let (Cart p _ _) = moveCart w c in Left p
        go [] r = Right r
        go (c:xs) r = let c' = moveCart w c in
                        if c' `elem` (r <> xs) then
                          go (filter (/= c') xs) (filter (/= c') r)
                        else go xs (c':r)

-- Part 2, but animated.  Also, it doesn't stop.
part2a :: IO ()
part2a = do
  w <- getInput
  putStr ("\ESC[2J" <> show w)

  go w
  pure ()

  where
    go :: World -> IO ()
    go w@(World m carts) = do
      removeCarts w
      let w'@(World _ carts') = moveCarts w
      addCarts w'
      hFlush stdout
      threadDelay (50000 `div` length carts')
      go w'

    removeCarts :: World -> IO ()
    removeCarts w@(World m carts) = do
      mapM_ (\(Cart (x,y) _ _) -> do
                putStr $ "\ESC[" <> show (y + 2) <> ";" <> show (x + 1) <> "H"
                putStr $ show (m Map.! (x,y))) carts

    addCarts :: World -> IO ()
    addCarts (World _ carts) =
      mapM_ (\c@(Cart (x,y) _ _) ->
                putStr $ mconcat ["\ESC[", show (y + 2), ";", show (x + 1), "H",
                                   "\ESC[41;1m", show c, "\ESC[0m"]) carts

    moveCarts :: World -> World
    moveCarts w@(World m carts) = World m $ go (sort carts) []

      where
        go :: [Cart] -> [Cart] -> [Cart]
        go [] r = r
        go (c:xs) r = let c' = moveCart w c in
                        if c' `elem` (r <> xs) then
                          go (filter (/= c') xs) (filter (/= c') r)
                        else go xs (c':r)
