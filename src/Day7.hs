{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Semigroup ((<>))

import Data.List (sort, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Graph as G
import qualified Data.Text.Encoding as E
import qualified Data.Attoparsec.Text as A
import Data.Text (Text, pack, unpack)

-- B requires A
data Requirement = Requirement Char Char deriving (Show)

-- ID, Remaining Time, Blockers
data Task = Task Char Int [Char] deriving (Eq, Ord, Show)

data State = Ready | Blocked | Done deriving (Eq, Ord, Show)

parseReq :: A.Parser Requirement
parseReq = Requirement <$ "Step " <*> A.anyChar
           <* " must be finished before step " <*> A.anyChar <* " can begin."

readInput :: IO [Requirement]
readInput = do
  (Right ls) <- A.parseOnly (parseReq `A.sepBy` (A.char '\n')) <$> pack <$> readFile "input/day7"
  pure ls

-- Figure out an order in which tasks can be performed.
trav :: Set.Set Char -> Map.Map Char [Char] -> [Char]
trav todo reqs = go todo mempty []
  where
    go :: Set.Set Char -> Set.Set Char -> [Char] -> [Char]
    go td done rv
      | null td = reverse rv
      | otherwise = let c = Set.findMin $ Set.filter ready td in
                      go (Set.delete c td) (Set.insert c done) (c : rv)

      where
        ready c = all (`elem` done) $ Map.findWithDefault mempty c reqs

cost :: Char -> Int
cost = (subtract 4) . fromEnum

state :: Task -> State
state (Task _ 0 _) = Done
state (Task _ _ []) = Ready
state _ = Blocked

-- Update tasks, remembering work that's been done and unblocking any
-- tasks that have completed.
uptasks :: [Task] -> [Task] -> [Task]
uptasks a b = map (clean.snd) $ Map.toList $ Map.fromList ((map k b) <> (map k a))
  where k t@(Task a _ _) = (a, t)
        clean (Task a n reqs) = Task a n (reqs \\ done)
        done :: [Char]
        done = concatMap (\t@(Task c _ _) -> if state t == Done then [c] else []) a

-- Allow n workers to perform work on the first n tasks that are
-- currently ready.
schedule :: [Task] -> Int -> (Int, [Task])
schedule tasks workers = let ready = sort $ filter ((== Ready) . state) tasks
                             doing = take workers ready
                             worked = minimum $ map (\(Task _ n _) -> n) doing in
                           (worked, uptasks (map (\(Task c n b) -> Task c (n - worked) b) doing) tasks)

-- Reschedule until all tasks are complete.
complete :: [Task] -> Int -> Int
complete tasks workers = go tasks 0
  where
    go [] n = n
    go t n = let (w, t') = schedule t workers in
               go (filter ((/= Done) . state) t') (n+w)

part1 :: IO ()
part1 = do
  inp <- readInput

  let s = Set.fromList $ concatMap (\(Requirement a b) -> [a, b]) inp

  let reqs = Map.fromListWith (<>) $ map (\(Requirement a b) -> (b, [a])) inp

  let l = trav s reqs

  print l

part2 :: IO ()
part2 = do
  inp <- readInput

  let s = Set.fromList $ concatMap (\(Requirement a b) -> [a, b]) inp
  let reqs = Map.fromListWith (<>) $ map (\(Requirement a b) -> (b, [a])) inp
  let tasks = map (\c -> Task c (cost c) (Map.findWithDefault mempty c reqs)) (Set.toList s)

  print $ complete tasks 5
