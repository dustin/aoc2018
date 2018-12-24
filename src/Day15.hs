{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day15 where

import           Data.Foldable   (minimumBy)
import           Data.List       (foldl', intercalate, sort, sortBy, sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (isJust)
import           Data.Ord        (comparing)
import qualified Data.PQueue.Min as Q
import qualified Data.Set        as Set
import           Data.Tuple      (swap)
import           Debug.Trace     (trace)

import           Search          (binSearch, dijkstra')

data Thing = Wall | Open | Elf Int | Goblin Int

instance Show Thing where
  show Wall       = "#"
  show Open       = "."
  show (Elf x)    = "E(" <> show x <> ")"
  show (Goblin x) = "G(" <> show x <> ")"

isOpen :: Thing -> Bool
isOpen Open = True
isOpen _    = False

isElf :: Thing -> Bool
isElf (Elf _) = True
isElf _       = False

isGoblin :: Thing -> Bool
isGoblin (Goblin _) = True
isGoblin _          = False

isEnemy :: Thing -> Thing -> Bool
isEnemy a b = (isElf a && isGoblin b) || (isGoblin a && isElf b)

hp :: Thing -> Int
hp (Elf x)    = x
hp (Goblin x) = x
hp x          = error ("no HP for " <> show x)

type HitFun = Thing -> Thing

mkHit :: Int -> Thing -> Thing
mkHit e = dohit

  where
    dohit :: Thing -> Thing
    dohit (Elf x)
      | x > 3 = Elf $ x - 3
      | otherwise = Open
    dohit (Goblin x)
      | x > e = Goblin $ x - e
      | otherwise = Open
    dohit x = error ("can't hit " <> show x)

newtype World = World (Map (Int,Int) Thing)

instance Show World where
  show (World m) =intercalate "\n" $ map row [0..my]

    where
      mx = maximum (fst <$> Map.keys m)
      my = maximum (snd <$> Map.keys m)
      row y = concatMap (\x -> take 1 . show $ m Map.! (x,y)) [0..mx] <> hps
        where hps = let obs = filter (\o -> isElf o || isGoblin o) $ map (\x -> m Map.! (x,y)) [0..mx] in
                      if null obs then ""
                      else " " <> intercalate ", " (map show obs)

parseInput :: [String] -> World
parseInput lns = World $ Map.fromList $ concatMap (\(y,r) -> map (\(x,c) -> ((x,y),p c)) $ zip [0..] r) $ zip [0..] lns

  where p :: Char -> Thing
        p 'E' = Elf 200
        p 'G' = Goblin 200
        p '.' = Open
        p _   = Wall

readingOrder :: (Int,Int) -> (Int,Int) -> Ordering
readingOrder = comparing swap

readingSort :: [(Int,Int)] -> [(Int,Int)]
readingSort = sortBy readingOrder

ofType :: World -> (Thing -> Bool) -> [(Int,Int)]
ofType (World m) f = readingSort . Map.keys . Map.filter f $ m

elves :: World -> [(Int,Int)]
elves w = ofType w isElf

goblins :: World -> [(Int,Int)]
goblins w = ofType w isGoblin

openSpace :: World -> Set.Set (Int,Int)
openSpace (World m) = Map.keysSet . Map.filter isOpen $ m

around :: (Int,Int) -> [(Int,Int)]
around (x,y) = [(x,y-1), (x-1,y), (x+1,y), (x,y+1)]

at :: World -> (Int,Int) -> Thing
at (World m) p = m Map.! p

mdist :: (Int,Int) -> (Int,Int) -> Int
mdist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

targets :: World -> (Int,Int) -> [(Int,Int)]
-- targets _ (23,8) = [(24,10)]
targets w@(World m) pos = let enemies = ofType w (isEnemy (m Map.! pos))
                              tset = openSpace w `Set.intersection` Set.fromList (concatMap around enemies) in
                            sortOn (mdist pos) (Set.toList tset)

players :: World -> [(Int,Int)]
players = readingSort . flip ofType (\x -> isGoblin x || isElf x)

bestMove :: World -> (Int,Int) -> Maybe (Int,Int)
bestMove w p
  | not $ null $ adjacentEnemies w p = Nothing
  | otherwise = let (m,_) = dijkstra' neighbors p (`elem` cands)
                    cands = targets w p
                    targs = sort $ map (\(Just j,x) -> (j,x)) $ filter (\(j,_) -> isJust j) $
                            map (\t -> (Map.lookup t m,t)) cands in
                  next $ best Nothing (candidate targs)

  where candidate :: [(Int,(Int,Int))] -> [(Int,Int)]
        candidate [] = []
        candidate [(_,x)] = [x]
        candidate ((s1,p1):xs) = p1 : map snd (takeWhile (\(x,_) -> x == s1) xs)

        best :: Maybe (Int,Int) -> [(Int,Int)] -> Maybe (Int,Int)
        best x [] = x
        best Nothing (p':xs) = best (Just p') xs
        best o@(Just a) (b:xs)
          | readingOrder (sub2 b p) (sub2 a p) == LT = best (Just b) xs -- better destination
          | otherwise = best o xs

        sub2 (a,b) (c,d) = (a-c, b-d)

        next Nothing  = Nothing
        next (Just x) = stepTo w p x

        ospac = openSpace w
        neighbors p' = map (1,) $ filter (`Set.member` ospac) (around p')

stepTo :: World -> (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
stepTo w f t
  | f `adjacentTo` t = Just t
  | otherwise = -- trace (" finding path to " <> show t) $
                let m = go (Q.singleton (0,t)) mempty mempty in
                  if null m then Nothing
                  else resolve f m

  where
    p1 `adjacentTo` p2 = p1 `elem` around p2
    ospac = openSpace w
    possible p = filter (`Set.member` ospac) (around p)

    resolve :: (Int,Int) -> Map.Map (Int,Int) Int -> Maybe (Int,Int)
    resolve p m
      | null next = Nothing
      | otherwise = Just (best next)
        where
          next :: [(Int,Int)]
          next = filter (`Map.member` m) (around p)
          best :: [(Int,Int)] -> (Int,Int)
          best = minimumBy (\a b -> let t1 = m Map.! a
                                        t2 = m Map.! b in
                                      compare t1 t2 <> readingOrder a b)

    go :: Q.MinQueue (Int,(Int,Int)) -> Map.Map (Int,Int) Int -> Set.Set (Int,Int) -> Map.Map (Int,Int) Int
    go q m seen
      | Q.null q = m
      | f `elem` around p = go odo (Map.insert p d m) seen
      | null ps = go odo m seen
      | otherwise = go (odo `Q.union` psd) (Map.insertWith min p d m) (Set.union seen (Set.fromList moves))

      where
        ([(d,p)], odo) = Q.splitAt 1 q
        moves = filter (\p' -> 1+d < Map.findWithDefault (1+d+1) p' m) ps
        ps = filter (`Set.notMember` seen) $ possible p
        psd = Q.fromList $ map (d+1,) moves


adjacentEnemies :: World -> (Int,Int) -> [(Int,Int)]
adjacentEnemies w p = filter (\x -> isEnemy (at w p) (at w x)) $ around p

attack :: World -> (Int,Int) -> HitFun -> World
attack (World m) p hit = World $ Map.adjust hit p m

move :: World -> (Int,Int) -> Maybe (Int,Int) -> World
move w _ Nothing = w
move w@(World m) p (Just x)
  | isOpen (at w p) = w
  | otherwise = World $ Map.insert x (at w p) (Map.insert p Open m)

action :: World -> (Int,Int) -> HitFun -> World
action w p hit = let (w', p') = case bestMove w p of
                                  Nothing     -> (w,p)
                                  np@(Just x) -> (move w p np, x) in
                   attackOne w' p' $ adjacentEnemies w' p'
  where
    attackOne :: World -> (Int,Int) -> [(Int,Int)] -> World
    attackOne w' _ [] = w'
    attackOne w'@(World m) _ ae = attack w' (best ae) hit
      where
        best :: [(Int,Int)] -> (Int,Int)
        best = minimumBy (\a b -> let a' = m Map.! a
                                      b' = m Map.! b in
                                    comparing hp a' b' <> readingOrder a b)

gameOver :: World -> Bool
gameOver (World m) = let (e,g) = foldr (\x o@(e',g') ->
                                           case () of _
                                                        | isElf x -> (e'+1,g')
                                                        | isGoblin x -> (e',g'+1)
                                                        | otherwise -> o)
                                   (0,0) m in
                         e == 0 || g == 0

aRound :: World -> HitFun -> (Bool, World)
aRound w hit = foldl' perform (False, w) (players w)

  where perform :: (Bool, World) -> (Int,Int) -> (Bool, World)
        perform (_,w') p =
          if hasEnemies w' p then (True, action w' p hit)
          else (False, w')

        hasEnemies :: World -> (Int,Int) -> Bool
        hasEnemies w' p = not.null $ ofType w' (isEnemy $ at w' p)

play' :: World -> Int -> HitFun -> (World -> Int -> World) -> (Int, World)
play' w i hit f = let (did,w') = aRound (f w i) hit in
                    if not did then (i,w')
                    else play' w' (i+1) hit f

play :: World -> Int -> HitFun -> (Int, World)
play w i hit = play' w i hit const

score :: World -> Int
score w = sum (map (hp . at w) $ players w)

getInput :: IO World
getInput = parseInput . lines <$> readFile "input/day15"

-- final score after 77 rounds: 2543
-- 195811
part1 :: IO ()
part1 = do
  w <- getInput
  print w
  let (r, w') = play' w 0 (mkHit 3) (\w'' i -> trace (show i <> "\n" <> show w'' <> "\n") w'')
  let s = score w'
  putStrLn $ "final score after " <> show r <> " rounds: " <> show s
  print w'
  print $ s * r

-- final score after 63 rounds: 1109
-- 69867
part2 :: IO ()
part2 = do
  w <- getInput
  let nelves = length $ elves w
  putStrLn ("Initial elves: " <> show nelves)

  let ans = binSearch (\i -> let (ok,w') = tilDeath w i nelves in
                               trace ("@" <> show i <> " elves: " <> show (length $ elves w') <> "\n" <> show w') $
                               if ok then GT
                               else LT) 4 100
  let (r, w') = play w 0 (mkHit ans)
  let s = score w'
  putStrLn $ "final score after " <> show r <> " rounds: " <> show s
  print w'
  print $ s * r


  where
    tilDeath :: World -> Int -> Int -> (Bool,World)
    tilDeath w n es = go w

      where
        hit = mkHit n
        go w'' = let (did,w') = aRound w'' hit in
                   case () of _
                                | not did -> (True,w')
                                | length (elves w') < es -> (False,w')
                                | otherwise -> go w'
