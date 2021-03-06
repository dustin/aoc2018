{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import           Data.Semigroup       ((<>))

import           Data.List            (sort, (\\))
import qualified Data.Map.Strict      as Map
import           Data.Word            (Word32)

import           Text.Megaparsec      (endBy)
import           Text.Megaparsec.Char (letterChar)

import           Advent.AoC           (Parser, parseFile)
import qualified Advent.BitSet        as BS

type CharSet = BS.BitSet Char Word32

-- B requires A
data Requirement = Requirement Char Char deriving (Show)

-- ID, Remaining Time, Blockers
data Task = Task Char Int [Char] deriving (Eq, Ord, Show)

data State = Ready | Blocked | Done deriving (Eq, Ord, Show)

parseReq :: Parser Requirement
parseReq = Requirement <$ "Step " <*> letterChar
           <* " must be finished before step " <*> letterChar <* " can begin."

readInput :: IO [Requirement]
readInput = parseFile (parseReq `endBy` "\n") "input/day7"

charSet :: [Char] -> CharSet
charSet = BS.fromList ('A', 'Z')

-- Figure out an order in which tasks can be performed.
trav :: CharSet -> Map.Map Char [Char] -> [Char]
trav todo reqs = go todo (BS.bitSet ('A', 'Z')) []
  where
    go :: CharSet -> CharSet -> [Char] -> [Char]
    go td done rv
      | BS.null td = reverse rv
      | otherwise = let c = BS.findMin $ BS.filter ready td in
                      go (BS.delete c td) (BS.insert c done) (c : rv)

      where
        ready c = all (`BS.member` done) $ Map.findWithDefault mempty c reqs

cost :: Char -> Int
cost = (subtract 4) . fromEnum

state :: Task -> State
state (Task _ 0 _)  = Done
state (Task _ _ []) = Ready
state _             = Blocked

-- Update tasks, remembering work that's been done and unblocking any
-- tasks that have completed.
uptasks :: [Task] -> [Task] -> [Task]
uptasks a b = map (clean.snd) $ Map.toList $ Map.fromList ((map k b) <> (map k a))
  where k t@(Task a' _ _) = (a', t)
        clean (Task a' n reqs) = Task a' n (reqs \\ done)
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

part1 :: [Requirement] -> String
part1 inp =
  let s = charSet $ concatMap (\(Requirement a b) -> [a, b]) inp
      reqs = Map.fromListWith (<>) $ map (\(Requirement a b) -> (b, [a])) inp in
    trav s reqs

part2 :: [Requirement] -> Int
part2 inp =
  let s = charSet $ concatMap (\(Requirement a b) -> [a, b]) inp
      reqs = Map.fromListWith (<>) $ map (\(Requirement a b) -> (b, [a])) inp
      tasks = map (\c -> Task c (cost c) (Map.findWithDefault mempty c reqs)) (BS.toList s) in
    complete tasks 5
