module Day1 where

import Search (findCycle)

getInput :: IO [Int]
getInput = map pn . lines <$> readFile "input/day1"
  where
    pn :: String -> Int
    pn ('+':xs) = read xs
    pn l        = read l

-- 576
part1 :: IO ()
part1 = print =<< sum <$> getInput

part2h :: [Int] -> Int
part2h l = let (_,_,x) = findCycle id $ scanl1 (+) (cycle l) in x

-- 77674
part2 :: IO ()
part2 =  print =<< part2h <$> getInput
