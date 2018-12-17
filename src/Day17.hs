{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Day17 where

import           Control.Applicative  ((<|>))
import           Control.Monad        (replicateM)
import qualified Data.Attoparsec.Text as A
import           Data.Bits            ((.&.), (.|.))
import           Data.List            (intercalate)
import qualified Data.Map.Strict      as Map
import qualified Data.Set as Set
import           Data.Text            (Text, pack)
import Debug.Trace (trace)

newtype Clay = Clay (Int,Int) deriving (Show)

newtype Scan = Scan (Map.Map (Int,Int) Char)

bounds :: Scan -> ((Int,Int),(Int,Int))
bounds (Scan m) = ((minimum xs,minimum ys),(maximum xs, maximum ys))
  where
    ks = Map.keys m
    xs = fst <$> ks
    ys = snd <$> ks
    mnx = minimum xs
    mny = minimum ys
    mxx = maximum xs
    mxy = maximum ys


instance Show Scan where
  show s@(Scan m) =  intercalate "\n" $ [firstrow] <> map row [mny..mxy]

    where
      ((mnx,mny),(mxx,mxy)) = bounds s
      firstrow = replicate (500 - mnx + 1) ' ' <> "+"

      row y = map (\x -> Map.findWithDefault '.' (x,y) m) [mnx - 1 ..mxx + 1]


parseScans :: A.Parser Scan
parseScans = do
  clays <- parseClay `A.sepBy` "\n"
  pure $ Scan $ Map.fromList (map (\(Clay x) -> (x,'#')) (mconcat clays))

parseClay :: A.Parser [Clay]
parseClay = do
  (axis1, points1) <- parseAxis <* ", "
  (axis2, points2) <- parseAxis

  let (xs, ys) = if axis1 == 'x' then (points1,points2) else (points2,points1)

  pure [Clay (x,y) | x <- xs, y <- ys]

  where
    parseAxis :: A.Parser (Char, [Int])
    parseAxis = do
      axis <- A.satisfy (`elem` ['x', 'y']) <* "="
      nums <- numSeq <|> aNum

      pure (axis, nums)

    aNum :: A.Parser [Int]
    aNum = (:[]) <$> A.decimal

    numSeq :: A.Parser [Int]
    numSeq = do
      a <- A.decimal <* ".."
      b <- A.decimal
      pure [a..b]

getInput :: IO (Either String Scan)
getInput = A.parseOnly parseScans . pack <$> readFile "input/day17"

pour :: Scan -> (Int,Int) -> Scan
pour s@(Scan m) (sx,sy) = Scan $ down (sx,sy) m

  where
    ((mnx,mny),(mxx,mxy)) = bounds s

    down (x,y) m'
      | y > mxy = m'
      -- | trace ("down from " <> show (x,y)) False = undefined
      | Map.lookup (x,y) m' `elem` [Nothing, Just '~'] = down (x,y+1) $ Map.insert (x,y) '|' m'
      | Map.lookup (x,y) m' == Just '#' = fill (x,y-1) m'
      | otherwise = m'

    fill (x,y) m'
      | y > mxy || y < mny = m'
      -- | x >= fmx || x <= fmn || y > mxy || y < mny = m'
      -- | trace ("filling " <> show (x,y)) False = undefined
      -- | trace (show $ Scan (Map.filterWithKey (\(_,ky) _ -> ky <= y) m')) False = undefined
      | ml (x,y+1) `notElem` [Just '#', Just '~'] = m'
      | ml (x,y) `elem` [Nothing, Just '|'] && spills = foldr down (fill' '~') spillsat
      | ml (x,y) `elem` [Nothing, Just '|'] = fill (x,y-1) $ fill' '~'
      | otherwise = m'

      where ml = (`Map.lookup` m')
            fill' c = Map.union (Map.fromList (map (,c) range)) m'
            range = [(x',y) | x' <- [fst edges .. snd edges]]
            spills = spillpoint (fst edges, y) || spillpoint (snd edges, y)
            spillsat = filter spillpoint [(fst edges,y), (snd edges,y)]
            spillpoint (x,y) = open (x,y+1)
            open p = ml p `elem` [Nothing, Just '|']
            edges = (search pred (x,y), search succ (x,y))
              where search f (x,y)
                      | spillpoint (f x,y) = f x
                      | ml (f x,y) == Just '#' = x
                      | otherwise = search f (f x,y)

countWater :: Scan -> Int
countWater (Scan m) = Map.foldr (\x o -> if x `elem` ['~', '|'] then o + 1 else o) 0 m

-- 33362
part1 :: IO ()
part1 = do
  (Right scans) <- getInput
  let ((mnx,mny),(mxx,mxy)) = bounds scans
  let s' = pour scans (500,mny)
  print s'
  print $ countWater s'

-- 30323 is too high
part2 :: IO ()
part2 = do
  (Right scans) <- getInput
  let ((mnx,mny),(mxx,mxy)) = bounds scans
  let (Scan m) = pour scans (500,mny)
  print $ Map.foldr (\x o -> if x == '~' then o + 1 else o) 0 m
