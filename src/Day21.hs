{-# LANGUAGE OverloadedStrings #-}

module Day21 where

import           Control.Applicative  (liftA3)
import qualified Data.Attoparsec.Text as A
import           Data.Bits            ((.&.), (.|.))
import           Data.List            (intercalate)
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import           Data.Text            (Text, pack, unpack)
import qualified Data.Vector          as V
import           Debug.Trace          (trace)

import Elfcode

getInput :: IO (Either String Program)
getInput = A.parseOnly parseProg . pack <$> readFile "input/day21"

findR5s :: Program -> [Int]
findR5s p = map (\(_,rs) -> reg rs 5) $ iterate (uncurry (execUntil p (\(ip,_) -> ip == 28))) (0,(0,0,0,0,0,0))

-- 3941014
part1 :: IO ()
part1 = do
  (Right p) <- getInput
  print $ (head . filter (/= 0) . findR5s) p


part2' :: Program -> Int
part2' = go mempty 0 . findR5s
  where
    go seen prev (x:xs)
      | Set.member x seen = prev
      | otherwise = go (Set.insert x seen) x xs

part2 :: IO ()
part2 = do
  (Right p) <- getInput
  print $ part2' p
