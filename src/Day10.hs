{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import           Control.Monad              (mapM_)
import           Data.List                  (sortOn)
import           Data.List.Extra            (chunksOf)
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import           AoC                        (Parser, parseFile)

-- position=< 9,  1> velocity=< 0,  2>

data Vec = Vec (Int,Int) (Int,Int) deriving (Eq, Ord, Show)

parseVec :: Parser Vec
parseVec = do
  x <- "position=<" *> num <* ","
  y <- num <* "> velocity=<"

  vx <- num <* ","
  vy <- num <* ">"

  pure $ Vec (x,y) (vx,vy)

  where num = space *> signed space decimal <* space

getInput :: IO [Vec]
getInput = parseFile "input/day10" (parseVec `endBy` "\n")

-- Move a vector to where it should be after n seconds
vecMove :: Int -> Vec -> Vec
vecMove by (Vec (x,y) v@(vx,vy)) = Vec (x + (vx*by), y + (vy*by)) v

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
                  (_, (maxx,maxy)) = bounds vs'
                  m = Map.fromList [((x,y),' ') | x <- [0..maxx], y <- [0..maxy]]
                  mm = chunksOf (maxx+1) $ map snd $ sortOn (snd . fst) $ Map.toList $ Map.union (Map.fromList $ map (\(Vec p _) -> (p,'#')) vs') m in
                mapM_ putStrLn mm

findMin :: Ord b => (a -> b) -> [a] -> a
findMin f (x:xs) = go xs x
  where go [] r = r
        go (x':xs') r
          | f x' > f r = r
          | otherwise = go xs' x'

part1 :: IO ()
part1 = do
  vs <- getInput
  let vss = map (\n -> let vs' = originate (vecMove n <$> vs) in (n, bounds vs', vs')) [1..100000]
  let (n,_,_) = findMin (\(_,b,_) -> b) vss
  print n
  drawVecs $ vecMove n <$> vs
