{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import qualified Data.Array      as A
import           Data.List       (intercalate)
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map

import           Elfcode         (align)
import           Search          (dijkstra)

data Type = Rocky | Wet | Narrow deriving (Eq, Ord, Enum, Bounded, Show)

type Cell = Int -- erosion level

type CaveSpec = (Int,Int,Int) -- X, Y, Depth

type XY = (Int,Int)

newtype Survey = Survey (A.Array XY Cell) deriving (Show)

data Tool = Gear | Torch | Neither deriving (Bounded, Ord, Enum, Eq, Show)

alles :: (Bounded a, Enum a) => [a]
alles = [minBound .. maxBound]

compatible :: Tool -> Type -> Bool
compatible Gear Rocky     = True
compatible Torch Rocky    = True
compatible Gear Wet       = True
compatible Neither Wet    = True
compatible Torch Narrow   = True
compatible Neither Narrow = True
compatible _ _            = False

compatibleWith :: Type -> [Tool]
compatibleWith t = filter (flip compatible t) alles

compatMap :: Map (Type,Type) Tool
compatMap = Map.fromList [((t1,t2),tool)
                         | t1 <- alles, t2 <- alles, tool <- alles,
                           t1 /= t2, compatible tool t1, compatible tool t2]

altTool :: Tool -> Type -> Type -> Tool
altTool t t1 t2 = head $ filter (\t' -> compatible t' t1 && compatible t' t2 && t /= t') alles

adjacent :: XY -> [XY]
adjacent (x,y) = [ (x,y+1), (x-1, y), (x+1, y), (x,y-1) ]

bounded :: XY -> [XY] -> [XY]
bounded (xmax,ymax) = filter (\(x,y) -> x >= 0 && y >= 0 && x <= xmax && y <= ymax)

survey :: CaveSpec -> (Int,Int) -> Survey
survey (tx,ty,depth) (w,h) = Survey $ mkArry

  where
    mkArry :: A.Array XY Cell
    mkArry = let a = A.array ((0,0),(w,h)) [((x,y), cell a (x,y)) | x <- [0..w], y <- [0..h]] in a

    cell :: A.Array XY Cell -> XY -> Cell
    cell a (x,y)
      | x == 0 && y == 0 = el 0
      | x == tx && y == ty = el 0
      | x == 0 = let g = y * 48271 in el g
      | y == 0 = let g = x * 16807 in el g
      | otherwise = let elx = a A.! (x-1,y)
                        ely = a A.! (x,y-1) in
                      el (elx * ely)

    el g = (g + depth) `mod` 20183

type XYT = (XY,Tool)

drawSurvey :: Survey -> Map XY Char -> String
drawSurvey s@(Survey a) overlay = intercalate "\n" $ map row [0..my]

    where
      (_,(mx,my)) = A.bounds a
      row y = map (\x -> ct (x,y)) [0..mx]

      ct p
        | Map.member p overlay = overlay Map.! p
        | otherwise = case cellType s p of
                        Rocky  -> '.'
                        Wet    -> '='
                        Narrow -> '|'


drawCostMap :: Map XYT Int -> String
drawCostMap m = hdr <> (align $ intercalate "\n" $ map row [0..my])
  where
    hdr = (show (alles :: [Tool])) <> "\n"
    ks = Map.keys m
    mx = maximum $ map (fst.fst) ks
    my = maximum $ map (snd.fst) ks

    row y = intercalate "\t" [cell (x,y) | x <- [0..mx]]

    cell (x,y) = intercalate "," [ns $ Map.findWithDefault (-1) ((x,y),t) m | t <- alles]

    ns (-1) = "∞"
    ns x    = show x

changeCost :: Int
changeCost = 8


neighbors :: Survey -> XYT -> [(Int,XYT)]
neighbors s@(Survey a) = moveswc
  where
    (_,abounds) = A.bounds a
    moveswc (p,t) = concatMap aMove (moves p)
      where
        moves = bounded abounds . adjacent
        aMove :: XY -> [(Int,XYT)]
        aMove xy
          | compatible t ct = [(1,(xy,t)), (changeCost,(xy, altTool t ct (cellType s xy)))]
          | otherwise = let nt = compatMap Map.! (cellType s p, ct) in [(changeCost,(xy,nt))]
            where ct = cellType s xy

pathOverlay :: XY -> [XYT] -> Map XY Char
pathOverlay dxy p = Map.fromList ((dxy,'@') : fmap (st <$>) p)
  where
    st :: Tool -> Char
    st Gear    = 'G'
    st Torch   = 'T'
    st Neither = 'X'

cellType :: Survey -> XY -> Type
cellType (Survey a) p = let el = a A.! p in cellType' el

cellType' :: Int -> Type
cellType' el = toEnum $ el `mod` 3

riskLevel :: Survey -> Int
riskLevel (Survey a) = sum $ fmap (fromEnum . cellType') a

myCave :: Survey
myCave = survey (9,731,11109) (9,731)

part1 :: IO ()
part1 = print $ riskLevel myCave

part2' :: Int
part2' = let frm = ((0,0),Torch)
             to = ((9,731),Torch)
             s = survey (9,731,11109) (9 + (7*4), 731 + (7*4))
             Just (n,_) = dijkstra (neighbors s) frm to in n

part2 :: IO ()
part2 = print part2'
