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

data Vec = Vec (Int,Int) (Int,Int) deriving (Show)

parseVec :: A.Parser Vec
parseVec = do
  x <- "position=<" *> num <* ","
  y <- num <* "> velocity=<"

  vx <- num <* ","
  vy <- num <* ">"

  pure $ Vec (x,y) (vx,vy)

  where num = A.skipSpace *> A.signed A.decimal <* A.skipSpace

getInput :: IO (Either String [Vec])
getInput = A.parseOnly (parseVec `A.sepBy` (A.char '\n'))<$> pack <$> readFile "input/day10"

vecMove :: Int -> Vec -> Vec
vecMove by (Vec (x,y) v@(vx,vy)) = Vec (x + (vx * by), y + (vy*by)) v

bounds :: [Vec] -> ((Int,Int),(Int,Int))
bounds vs = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    xs :: [Int]
    xs = map (\(Vec (x,_) _) -> x) vs
    ys :: [Int]
    ys = map (\(Vec (_,y) _) -> y) vs

drawVecs :: [Vec] -> IO ()
drawVecs vs = let bs@((minx,miny), (maxx,maxy)) = bounds vs
                  m = Map.fromList $ [((x,y),' ') | x <- [0..maxx], y <- [0..maxy]]
                  mm = chunksOf (maxx+1) $ map snd $ sortBy (comparing (snd . fst)) $ Map.toList $ Map.union (Map.fromList $ map (\(Vec p _) -> (p,'#')) vs) m in
                mapM_ putStrLn mm

part1 :: IO ()
part1 = do
  (Right vs) <- getInput
  let candidates = filter (\x -> let ((minx,maxx),(miny,maxy)) = bounds $ vecMove x <$> vs in
                                minx >= 0 && miny >= 0) [0..100000]
  mapM_ (\n -> do
            print n
            let vs' = vecMove n <$> vs
            print $ bounds $ vs'
            drawVecs vs') candidates
