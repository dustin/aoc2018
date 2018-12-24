{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import           Control.Applicative  ((<|>))
import qualified Data.Map.Strict      as Map

import qualified Data.Attoparsec.Text as A
import           Data.Functor         (($>))
import           Data.List            (sort)
import           Data.Text            (pack)

{-
-- not a leap year...

[1518-06-23 00:43] wakes up
[1518-06-01 00:26] wakes up
[1518-08-29 00:02] falls asleep
[1518-03-06 00:02] Guard #1783 begins shift
-}

mondays :: [Int]
mondays = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

mdayo :: [Int]
mdayo = scanl (+) 0 mondays

data Deet = Begins Int | Asleep | Awake deriving (Eq, Show)

newtype TS = TS (Int,Int,Int,Int) deriving (Ord, Eq, Show)

instance Enum TS where
  fromEnum (TS (mon,day,h,m)) = (mdayo !! mon + day) * 1440 + (h * 60) + m
  toEnum x = let (md, hm) = x `divMod` 1440
                 (h,m) = hm `divMod` 60
                 mons = takeWhile (< md) mdayo
                 mon = length mons - 1 in
               TS (mon, md - last mons, h, m)

data Event = Event TS Deet deriving (Eq, Show)

instance Ord Event where
  compare (Event a _) (Event b _) = compare a b

anEvent :: A.Parser Event
anEvent = Event <$> timestamp <*> deet

  where deet :: A.Parser Deet
        deet = ("wakes up" $> Awake)
               <|> ("falls asleep" $> Asleep)
               <|> Begins <$> ("Guard #" *> A.decimal <* " begins shift")

        timestamp :: A.Parser TS
        timestamp = do
          mon <- "[1518-" *> A.decimal <* "-"
          day <- A.decimal <* " "
          h <- A.decimal <* ":"
          m <- A.decimal <* "]" <* " "

          pure $ TS (mon, day, h, m)

tsDiff :: TS -> TS -> Int
tsDiff ta tb = fromEnum ta - fromEnum tb

data GuardInfo = GuardInfo TS Int [(TS, Deet)] deriving (Show)

grp :: [Event] -> [GuardInfo]
grp = go []

  where
    go :: [GuardInfo] -> [Event] -> [GuardInfo]
    go gi [] = gi
    go gi (Event ts (Begins x):xs) = go ((GuardInfo ts x []) : gi) xs
    go ((GuardInfo gts gid evs):rest) (Event ts dt:xs) = go ((GuardInfo gts gid (evs <> pure (ts,dt))) : rest) xs
    go _ _ = undefined

sleepMins :: GuardInfo -> [(TS,TS)] -- (Asleep, Awake)
sleepMins (GuardInfo ts _ evs) = awake ts [] evs

  where
    awake _ x []                 = x
    awake _ as ((t,Asleep):rest) = asleep t as rest

    asleep _ x []                  = x
    asleep ts' rs ((t,Awake):rest) = awake t ((ts',t):rs) rest

sleepTime :: GuardInfo -> Int
sleepTime gi = foldr (\(l,h) o -> o + length [l..h] - 1) 0 $ sleepMins gi

justMins :: GuardInfo -> [Int]
justMins gi = concatMap (\(l,h) -> tail.reverse $ map (\(TS (_,_,_,m)) -> m) [l..h]) $ sleepMins gi

part1 :: IO ()
part1 = do
  (Right ls) <- A.parseOnly (anEvent `A.sepBy` (A.char '\n')) <$> pack <$> readFile "input/day4"
  let grpd = grp (sort ls)
  let gsleeps = reverse . sort $ map (\g@(GuardInfo _ i _) -> (i, sleepTime g)) grpd
  let totals = Map.fromListWith (+) gsleeps
  let [(sleepiest,_)] = Map.toList $ Map.filter (\x -> x == maximum totals) totals
  let offenses = concat $ justMins <$> filter (\(GuardInfo _ i _) -> i == sleepiest) grpd
  let counts = Map.fromListWith (+) $ map (\x -> (x,1)) offenses
  let [(minute,_)] = Map.toList $ Map.filter (\x -> x == maximum counts) counts
  print $ show sleepiest <> " * " <> show minute <> " = " <> show (sleepiest * minute)


part2 :: IO ()
part2 = do
  (Right ls) <- A.parseOnly (anEvent `A.sepBy` (A.char '\n')) <$> pack <$> readFile "input/day4"
  let grpd = grp (sort ls)
  let gsleeps = map (\g@(GuardInfo _ i _) -> (i, justMins g)) grpd
  let m = Map.fromListWith (Map.unionWith (+)) $ concatMap (\(g, mins) -> map (\m' -> (g,Map.singleton m' 1)) mins) gsleeps

  let (g,mn,_) = Map.foldrWithKey (\k mm o@(_,_,n) ->
                                     case Map.toList $ Map.filter (\x -> x == maximum mm) mm of
                                       [(m'',n')] -> if n' > n then (k,m'',n') else o
                                       _ -> o
                                  ) (0,0,0) m
  print $ show g <> " * " <> show mn <> " = " <> show (mn*g)
