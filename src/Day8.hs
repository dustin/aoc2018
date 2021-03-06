{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import           Control.Monad              (replicateM)
import           Control.Monad.Trans.State  (State, evalState, get, put)
import           Data.Foldable              (fold)
import           Data.List                  (mapAccumL)
import qualified Data.Tree                  as Tree
import           Text.Megaparsec.Char       (space)
import           Text.Megaparsec.Char.Lexer (decimal)

import           Advent.AoC                 (Parser, parseFile)

readInput :: IO (Tree.Tree [Int])
readInput =  readTree <$> map read <$> words <$> readFile "input/day8"

-- 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
-- A----------------------------------
--     B----------- C-----------
--                      D-----


-- [2,3,0,3,10,11,12,1,1,0,1,99,2,1,1,2]
-- A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
-- B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
-- C, which has 1 child node (D) and 1 metadata entry (2).
-- D, which has 0 child nodes and 1 metadata entry (99).

readTree :: [Int] -> Tree.Tree [Int]
readTree (c:m:xs) =
  let (xs', subs) = mapAccumL (\xs'' _ ->
                                  let t = readTree xs'' in
                                    (drop (sz t) xs'', t)) xs [1..c] in
    Tree.Node (take m xs') subs

  where
    sz :: Tree.Tree [Int] -> Int
    sz (Tree.Node a bs) = foldr (\x o -> o + sz x) (2 + length a) bs

-- Variant of readTree using State monad.  More code, but a lot easier
-- to understand.
readTree' :: [Int] -> Tree.Tree [Int]
readTree' = evalState getTree
  where getTree :: State [Int] (Tree.Tree [Int])
        getTree = do
          n <- get1
          m <- get1
          a <- replicateM n getTree
          b <- replicateM m get1
          pure (Tree.Node b a)

        get1 :: State [a] a
        get1 = do
          xxs <- get
          case xxs of
            x:xs -> x <$ put xs
            []   -> error "get1: empty list"

-- Or just parse it directly into the right structure.
parseTree :: Parser (Tree.Tree [Int])
parseTree = do
  c <- aNum
  m <- aNum
  cs <- replicateM c parseTree
  ms <- replicateM m aNum

  pure $ Tree.Node ms cs

  where
    aNum :: Parser Int
    aNum = decimal <* space

readInput' :: IO (Tree.Tree [Int])
readInput' = parseFile parseTree "input/day8"

-- 42951
part1 :: IO ()
part1 = print =<< (sum .concat . Tree.flatten) <$> readInput

part1' :: IO ()
part1' = print =<< sum.fold <$> readInput'

count :: Tree.Tree [Int] -> Int
count (Tree.Node xs []) = sum xs
count (Tree.Node xs subs) = foldr (\x o -> o + count (subs `at` x)) 0 xs

  where at :: Tree.Forest [Int] -> Int -> Tree.Tree [Int]
        at l i
          | i > length l = Tree.Node [] []
          | otherwise = l !! (i-1)

-- 18568
part2 :: IO ()
part2 = print =<< count <$> readInput

