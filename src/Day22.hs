{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import qualified Data.Array          as A

data Type = Rocky | Wet | Narrow deriving (Eq, Enum, Bounded, Show)

type Cell = (Int,Int) -- geologic index, erosion level

type CaveSpec = (Int,Int,Int) -- X, Y, Depth

newtype Survey = Survey (A.Array (Int,Int) Cell) deriving (Show)

survey :: CaveSpec -> Survey
survey (tx,ty,depth) = Survey $ mkArry

  where
    mkArry :: A.Array (Int,Int) Cell
    mkArry = let a = A.array ((0,0),(tx,ty)) [((x,y), cell a (x,y)) | x <- [0..tx], y <- [0..ty]] in a

    cell :: A.Array (Int,Int) Cell -> (Int,Int) -> Cell
    cell a (x,y)
      | x == 0 && y == 0 = (0, el 0)
      | x == tx && y == ty = (0, el 0)
      | x == 0 = let g = y * 48271 in (g, el g)
      | y == 0 = let g = x * 16807 in (g, el g)
      | otherwise = let (_,elx) = a A.! (x-1,y)
                        (_,ely) = a A.! (x,y-1)
                        g = elx * ely in (g, el g)

    el g = (g + depth) `mod` 20183

cellType :: Survey -> (Int,Int) -> Type
cellType (Survey a) p = let (_,el) = a A.! p in cellType' el

cellType' :: Int -> Type
cellType' el = case el `mod` 3 of
                 0 -> Rocky
                 1 -> Wet
                 2 -> Narrow

riskLevel :: Survey -> Int
riskLevel (Survey a) = sum $ fmap (fromEnum . cellType' . snd) a

myCave :: Survey
myCave = survey (9,731,11109)

part1 :: IO ()
part1 = do
  print $ riskLevel myCave

