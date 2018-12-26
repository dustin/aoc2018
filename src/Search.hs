{-|
Module      : Search
Description : Searching stuff for AoC.
Copyright   : (c) Dustin Sallings, 2018
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

Things I use for searching space in AoC.
-}
module Search (dijkstra', dijkstra, resolveDijkstra, binSearch, findCycle) where

import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Min as Q
import qualified Data.Set        as Set


-- Get the position of the start of the first cycle and the cycle length from a list
findCycle :: Ord b => (a -> b) -> [a] -> (Int,Int)
findCycle f = go 0 mempty
  where go n mem (x:xs) = case Map.lookup t mem of
                            Nothing -> go (n+1) (Map.insert t n mem) xs
                            Just o  -> (o,n - o)
          where t = f x

-- Tests for this are in Day22.

-- | 'dijkstra'' uses [Dijkstra's
-- Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to
-- find the costs and links from a starting point to various points on
-- a graph.
--
-- See 'resolveDijkstra' for a means of determining a path from the
-- resulting values.
dijkstra' :: Ord v => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
  -> v -- ^ The starting point.
  -> (v -> Bool) -- ^ Predicate allowing early termination of search (if True).
  -> (Map v Int, Map v v) -- ^ (cost from origin to v, links from points to points)
dijkstra' neighbrf start done = go (Q.singleton (0,start)) (Map.singleton start 0) mempty mempty
  where
    go q m l seen
      | Q.null q = (m,l)
      | done pt = (m',l)
      | Set.member pt seen = go odo m l seen
      | otherwise = go (odo <> psd) m' l' (Set.insert pt seen)

      where
        ([(d,pt)], odo) = Q.splitAt 1 q
        moves = filter (\(c,p') -> c+d < Map.findWithDefault (c+d+1) p' m) (neighbrf pt)
        m' = Map.union (Map.fromList $ map (\(c,p') -> (p',c+d)) moves) m
        l' = Map.union (Map.fromList $ map (\(_,p') -> (p',pt)) moves) l
        psd = Q.fromList $ fmap (\(c,x) -> (d+c,x)) moves

-- | Using maps computed by 'dijkstra'', find the cost and path from the
-- start to a destination.
resolveDijkstra :: Ord v => Map v Int -> Map v v -> v -> v -> Maybe (Int,[v])
resolveDijkstra m l start end = case Map.lookup end m of
                                  Nothing   -> Nothing
                                  Just cost -> Just (cost, reverse $ end : go end)
  where
    go pt
      | pt == start = []
      | otherwise = next : go next
      where next = l Map.! pt

-- | 'dijkstra' uses [Dijkstra's
-- Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm) to
-- find the lowest cost path to a given destination.
dijkstra :: Ord v => (v -> [(Int,v)]) -- ^ Provide a list of all neighbors of v with their associated costs.
         -> v -- ^ The starting point.
         -> v -- ^ The destination point.
         -> Maybe (Int,[v])  -- ^ The cost to the destination, and the path to get there.
dijkstra neighbrf start end = resolve (dijkstra' neighbrf start (== end))
  where resolve (m,l) = resolveDijkstra m l start end

-- | 'binSearch' performs a binary search to find the boundary
-- function where a function returns its highest 'LT' value.
binSearch :: Integral a => (a -> Ordering) -> a -> a -> a
binSearch f l h
  | h < l     = l
  | v == GT   = binSearch f l (mid-1)
  | v == LT   = binSearch f (mid+1) h
  | otherwise = mid
  where
    mid = l + (h-l) `div` 2
    v = f mid
