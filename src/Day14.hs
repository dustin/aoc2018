{-# LANGUAGE OverloadedStrings #-}

module Day14 where

import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq

wtfseq :: Int -> Int -> (Seq.Seq Int -> Maybe a) -> a
wtfseq a b f = fromJust $ go (Seq.fromList [a,b]) (0,1)

  where go s (e1,e2) = f s <|> let s1 = (Seq.index s e1)
                                   s2 = (Seq.index s e2)
                                   (d1,d2) = (s1 + s2) `divMod` 10
                                   toAdd = Seq.fromList $ if d1 == 0 then [d2] else [d1, d2]
                                   s' = s <> toAdd
                                   e1' = (e1 + 1 + s1) `mod` Seq.length s'
                                   e2' = (e2 + 1 + s2) `mod` Seq.length s' in
                                 go s' (e1', e2')

-- 8176111038
part1 :: IO ()
part1 = putStrLn (concatMap show $ wtelf 3 7 890691)

  where
    wtelf :: Int -> Int -> Int -> [Int]
    wtelf a b n = wtfseq a b (\s -> if length s > n + 10
                                    then Just (toList . Seq.take 10 . Seq.drop n $ s)
                                    else  Nothing)


-- 20225578
part2 :: IO ()
part2 = print $ wtelf 3 7 $ map digitToInt "890691"

  where
    wtelf :: Int -> Int -> [Int] -> Int
    wtelf a b m = wtfseq a b (match m)

    match :: [Int] -> Seq.Seq Int -> Maybe Int
    match m s = let base = Seq.length s - (length m + 2)
                    (_,end) = Seq.splitAt base s in
                  at base m (toList end)

    at :: Int -> [Int] -> [Int] -> Maybe Int
    at _ _ [] = Nothing
    at b m s = if isPrefixOf m s
               then Just b
               else at (b+1) m (tail s)
