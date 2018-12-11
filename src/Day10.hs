{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Debug.Trace (trace)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Data.Either (either)
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad (mapM_)
import Data.List.Extra (chunksOf)

-- position=< 9,  1> velocity=< 0,  2>

data Vec = Vec (Int,Int) (Int,Int) deriving (Eq, Ord, Show)

parseVec :: A.Parser Vec
parseVec = do
  x <- "position=<" *> num <* ","
  y <- num <* "> velocity=<"

  vx <- num <* ","
  vy <- num <* ">"

  pure $ Vec (x,y) (vx,vy)

  where num = A.skipSpace *> A.signed A.decimal <* A.skipSpace

getInput :: IO (Either String [Vec])
getInput = A.parseOnly (parseVec `A.sepBy` A.char '\n') . pack <$> readFile "input/day10"

-- Move a vector to where it should be after n seconds
vecMove :: Int -> Vec -> Vec
vecMove by (Vec (x,y) v@(vx,vy)) = Vec (x + (vx * by), y + (vy*by)) v

-- Translate a vector by a given x and y value
vecTrans :: (Int,Int) -> Vec -> Vec
vecTrans (x,y) (Vec (px,py) v) = Vec (px+x,py+y) v

-- Move a list of vectors so that the min bound is origin
originate :: [Vec] -> [Vec]
originate vs = let ((minx,miny),_) = bounds vs in vecTrans (-minx, -miny) <$> vs

bounds :: [Vec] -> ((Int,Int),(Int,Int))
bounds vs = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs :: [Int]
    xs = map (\(Vec (x,_) _) -> x) vs
    ys :: [Int]
    ys = map (\(Vec (_,y) _) -> y) vs

drawVecs :: [Vec] -> IO ()
drawVecs vs = let vs' = originate vs
                  bs@((minx,miny), (maxx,maxy)) = bounds vs'
                  m = Map.fromList [((x,y),' ') | x <- [0..maxx], y <- [0..maxy]]
                  mm = chunksOf (maxx+1) $ map snd $ sortBy (comparing (snd . fst)) $ Map.toList $ Map.union (Map.fromList $ map (\(Vec p _) -> (p,'#')) vs') m in
                mapM_ putStrLn mm

findMin :: Ord b => (a -> b) -> [a] -> a
findMin f (x:xs) = go xs x
  where go [] r = r
        go (x':xs') r
          | f x' > f r = r
          | otherwise = go xs' x'

part1 :: IO ()
part1 = do
  (Right vs) <- getInput
  let n = findMin (\n -> let vs' = originate (vecMove n <$> vs)
                             (_,(w,h)) = bounds vs' in w*h) [1..100000]
  print $ n
  drawVecs $ vecMove n <$> vs
