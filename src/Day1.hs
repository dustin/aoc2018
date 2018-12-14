module Day1 where

import qualified Data.Set as Set

pn :: String -> Int
pn ('+':xs) = read xs
pn l        = read l

-- 576
part1 :: IO ()
part1 = print =<< sum . map pn . lines <$> readFile "input/day1"

part2h :: [Int] -> Int
part2h = folder mempty 0 . cycle
  where
    folder :: Set.Set Int -> Int -> [Int] -> Int
    folder s i (x:xs)
      | ix `Set.member` s = ix
      | otherwise = folder (Set.insert ix s) ix xs

      where ix = i + x

-- 77674
part2 :: IO ()
part2 =  print =<< (part2h . map pn . lines <$> readFile "input/day1")
