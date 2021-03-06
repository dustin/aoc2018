{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import           Data.Char     (toLower, toUpper)
import           Data.Foldable (minimumBy)
import qualified Data.Set      as Set

chain :: String -> String
chain [] = []
chain [x] = [x]
chain (x:y:xs)
  | match x y = chain xs
  | otherwise = x : chain (y:xs)

  where match x' y' = (x' /= y') && toLower x' == toLower y'

react :: String -> String
react s = let c = chain s in if s == c then c else react c

-- from glguy... took me a sec.
foldrthing :: String -> String
foldrthing = foldr step ""
  where
    step x (y:ys) | x /= y && toUpper x == toUpper y = ys
    step x ys     = x : ys

works :: Bool
works = react "dabAcCaCBAcCcaDA" == "dabCBAcaDA"

part1 :: IO ()
part1 = do
  datas <- filter (`notElem` ['\n']) <$> readFile "input/day5"
  print $ length $ react datas

part2 :: IO ()
part2 = do
  datas <- react <$> filter (`notElem` ['\n']) <$> readFile "input/day5"
  let chars = Set.fromList (toLower <$> datas)

  let m = map (\r -> (r, filter (\c -> toLower c /= r) datas)) (Set.toList chars)

  let mm = map (\(c, i) -> (c, length $ react i)) m

  let ans  = minimumBy (\(_,al) (_,bl) -> compare al bl) mm

  print ans
