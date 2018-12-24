{-# LANGUAGE TupleSections #-}

module Day2 where

import           Control.Monad     (guard)
import qualified Data.Map.Strict   as Map
import           Text.EditDistance (defaultEditCosts, levenshteinDistance)

checksum :: [String] -> Int
checksum lns =
  -- this was kind of gross, but it was the first thing I could think of.
  let w = (Map.fromListWith (+) . map (,1) <$> lns) :: [Map.Map Char Int]
      (t,f) = foldr (\m (t',f') -> (t' + if null $ Map.filter (==2) m then 0 else 1,
                                    f' + if null $ Map.filter (==3) m then 0 else 1)) (0,0) w in
    t*f

part1 :: IO ()
part1 = print =<< checksum <$> lines <$> readFile "input/day2"

match :: [String] -> [String]
match xs = do
  l1 <- xs
  l2 <- xs

  -- There's really a special case that can apply here since the
  -- distance we're looking for is one as a single character
  -- replacement.  Could've just done a
  -- length . filter (uncurry (/=)) $ zip l1 l2
  guard $ levenshteinDistance defaultEditCosts l1 l2 == 1

  pure $ map fst $ filter (uncurry (==)) $ zip l1 l2

part2 :: IO ()
part2 = print =<< head . match <$> lines <$> readFile "input/day2"
