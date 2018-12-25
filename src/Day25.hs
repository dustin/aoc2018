{-# LANGUAGE OverloadedStrings #-}

module Day25 where

import           Data.Graph                 (Graph)
import qualified Data.Graph                 as Graph
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy, optional)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import           AoC                        (Parser, parseFile)

type XYZT = (Int,Int,Int,Int)

parseQuads :: Parser [XYZT]
parseQuads = parseQuad `endBy` "\n"
  where
    parseQuad = (,,,) <$> num <*> num <*> num <*> num
    num = space *> signed space decimal <* optional ","

getInput' :: String -> IO [XYZT]
getInput' = parseFile parseQuads

mdist :: XYZT -> XYZT -> Int
mdist (x1,y1,z1,t1) (x2,y2,z2,t2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (t1 - t2)

graph :: [XYZT] -> Graph
graph ps = g
  where
    (g, _, _) = Graph.graphFromEdges $ map (\x -> (x, x, Map.findWithDefault [] x conns)) ps
    conns = Map.fromListWith (<>) [(x,[y]) | x <- ps, y <- ps, x < y, mdist x y <= 3]

nConstellations :: [XYZT] -> Int
nConstellations = length . Graph.components . graph
