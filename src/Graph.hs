module Graph where

import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Min as Q
import qualified Data.Set        as Set

-- Tests for this are in Day22.

-- This gives the costs to points and the link maps
dijkstra' :: Ord v => (v -> [(Int,v)]) -> v -> (v -> Bool) -> (Map v Int, Map v v)
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

dijkstra :: Ord v => (v -> [(Int,v)]) -> v -> v -> Maybe (Int,[v])
dijkstra neighbrf start end = resolve (dijkstra' neighbrf start (== end))

  where
    resolve (m,l) = case Map.lookup end m of
                      Nothing   -> Nothing
                      Just cost -> Just (cost, reverse $ end : go end)
      where
        go pt
          | pt == start = []
          | otherwise = next : go next
          where next = l Map.! pt
