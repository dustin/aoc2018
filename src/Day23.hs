{-# LANGUAGE OverloadedStrings #-}

module Day23 where

import qualified Data.Attoparsec.Text as A
import           Data.Foldable        (maximumBy)
import           Data.Foldable        (foldl')
import           Data.Ord             (comparing)
import           Data.Text            (pack)

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
getInput = getInput' "input/day23"

getInput' :: String -> IO (Either String [Nanobot])
getInput' fn = A.parseOnly (parseBot `A.sepBy` "\n") . pack <$> readFile fn

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

sumIf :: (a -> Bool) -> [a] -> Int
sumIf f = foldl' (\o x -> o + if f x then 1 else 0) 0

countInRangeOf :: (Int,Int,Int) -> [Nanobot] -> Int
countInRangeOf p = sumIf (flip inRange (Nanobot p 0))

mostReachablePoint :: [Nanobot] -> (Int,Int,Int)
mostReachablePoint bots = snd . maximum $ map (\p -> (countInRangeOf p bots,p)) proposals

  where
    proposals :: [(Int,Int,Int)]
    proposals = concatMap candidatesFor bots
      where
        candidatesFor (Nanobot (x,y,z) r) =
          [ (x+r, y, z), (x-r, y, z),
            (x, y+r, z), (x, y-r, z),
            (x, y, z+r), (x, y, z-r) ]

tripop :: (a -> b) -> (a,a,a) -> (b,b,b)
tripop f (a,b,c) = (f a, f b, f c)

-- {max,min}imize function, moving in the specified direction
imize :: ((Int,Int,Int) -> (Int,Int,Int) -> Bool) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
imize f = go
  where go :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
        go guess (0,0,0) = guess
        go guess@(x,y,z) by@(xb,yb,zb) = case candidates of
                                           [] -> go guess (tripop (`quot` 2) by)
                                           (p:_) -> go p by
          where
            candidates :: [(Int,Int,Int)]
            candidates = filter (f guess)
                         [ (x+xb, y+yb, z+zb),
                           (x+xb, y+yb, z),    (x+xb, y,    z+zb), (x, y+yb, z+zb),
                           (x+xb, y,    z),    (x,    y+yb, z),    (x, y, z+zb)]

towardsZero :: [Nanobot] -> (Int,Int,Int) -> Int -> (Int,Int,Int)
towardsZero bots point by = imize notWorse point (-by,-by,-by)
  where
    notWorse prev next = countInRangeOf next bots >= countInRangeOf prev bots

part2' :: [Nanobot] -> Int
part2' bots = distance (0,0,0) $ towardsZero bots (mostReachablePoint bots) 100000

part2 :: IO ()
part2 = do
  (Right bots) <- getInput' "input/day23"
  print $ part2' bots
