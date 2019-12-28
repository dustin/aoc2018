{-# LANGUAGE OverloadedStrings #-}

module Day25 where

import           Data.Graph                 (Graph)
import qualified Data.Graph                 as Graph
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy, optional)
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal, signed)

import           Advent.AoC                 (Parser, mdist4, parseFile)

type XYZT = (Int,Int,Int,Int)

parseQuads :: Parser [XYZT]
parseQuads = parseQuad `endBy` "\n"
  where
    parseQuad = (,,,) <$> num <*> num <*> num <*> num
    num = space *> signed space decimal <* optional ","

getInput' :: String -> IO [XYZT]
getInput' = parseFile parseQuads

graph :: [XYZT] -> Graph
graph ps = g
  where
    (g, _, _) = Graph.graphFromEdges $ map (\x -> (x, x, Map.findWithDefault [] x conns)) ps
    conns = Map.fromListWith (<>) [(x,[y]) | x <- ps, y <- ps, x < y, mdist4 x y <= 3]

nConstellations :: [XYZT] -> Int
nConstellations = length . Graph.components . graph
