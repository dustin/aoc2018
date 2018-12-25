{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day17 where

import           Codec.Picture              (PixelRGB8 (..), generateImage,
                                             writePng)
import           Control.Applicative        ((<|>))
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Text.Megaparsec            (endBy, satisfy, try)
import           Text.Megaparsec.Char.Lexer (decimal)

import           AoC                        (Parser, parseFile)

newtype Clay = Clay (Int,Int) deriving (Show)

newtype Scan = Scan (Map.Map (Int,Int) Char)

bounds :: Scan -> ((Int,Int),(Int,Int))
bounds (Scan m) = ((minimum xs,minimum ys),(maximum xs, maximum ys))
  where
    ks = Map.keys m
    xs = fst <$> ks
    ys = snd <$> ks


instance Show Scan where
  show s@(Scan m) =  intercalate "\n" $ [firstrow] <> map row [mny..mxy]

    where
      ((mnx,mny),(mxx,mxy)) = bounds s
      firstrow = replicate (500 - mnx + 1) ' ' <> "+"

      row y = map (\x -> Map.findWithDefault '.' (x,y) m) [mnx - 1 ..mxx + 1]


parseScans :: Parser Scan
parseScans = do
  clays <- parseClay `endBy` "\n"
  pure $ Scan $ Map.fromList (map (\(Clay x) -> (x,'#')) (mconcat clays))

parseClay :: Parser [Clay]
parseClay = do
  (axis1, points1) <- parseAxis <* ", "
  (_, points2) <- parseAxis

  let (xs, ys) = if axis1 == 'x' then (points1,points2) else (points2,points1)

  pure [Clay (x,y) | x <- xs, y <- ys]

  where
    parseAxis :: Parser (Char, [Int])
    parseAxis = do
      axis <- satisfy (`elem` ['x', 'y']) <* "="
      nums <- try numSeq <|> aNum

      pure (axis, nums)

    aNum :: Parser [Int]
    aNum = (:[]) <$> decimal

    numSeq :: Parser [Int]
    numSeq = do
      a <- decimal <* ".."
      b <- decimal
      pure [a..b]

getInput :: IO Scan
getInput = getInput' "input/day17"

getInput' :: String -> IO Scan
getInput' fn = parseFile fn parseScans

pour :: Scan -> Scan
pour s@(Scan m) = Scan $ down (500,mny) m

  where
    ((_,mny),(_,mxy)) = bounds s

    down (x,y) m'
      | y > mxy = m'
      | Map.lookup (x,y) m' `elem` [Nothing, Just '~'] = down (x,y+1) $ Map.insert (x,y) '|' m'
      | Map.lookup (x,y) m' == Just '#' = fill (x,y-1) m'
      | otherwise = m'

    fill (x,y) m'
      | y > mxy || y < mny = m'
      | ml (x,y+1) `notElem` [Just '#', Just '~'] = m'
      | ml (x,y) `elem` [Nothing, Just '|'] && spills = foldr down (fill' '|') spillsat
      | ml (x,y) `elem` [Nothing, Just '|'] = fill (x,y-1) $ fill' '~'
      | otherwise = m'

      where ml = (`Map.lookup` m')
            fill' c = Map.union (Map.fromList (map (,c) range)) m'
            range = [(x',y) | x' <- [(fst . head) edges .. (fst . last) edges]]
            spills = any spillpoint edges
            spillsat = fmap (+1) <$> filter spillpoint edges
            spillpoint (x',y') = ml (x',y'+1) == Nothing
            edges = [search pred x, search succ x]
              where search f x'
                      | spillpoint (f x',y) = (f x',y)
                      | ml (f x',y) == Just '#' = (x',y)
                      | otherwise = search f (f x')

countWater :: Scan -> Int
countWater (Scan m) = length . Map.filter (`elem` ['~', '|']) $ m

-- 33362
part1 :: IO ()
part1 = do
  scans <- getInput
  let s' = pour scans
  print s'
  print $ countWater s'

countWater2 :: Scan -> Int
countWater2 (Scan m) = length . Map.filter (== '~') $ m

-- 27801
part2 :: IO ()
part2 = do
  scans <- getInput
  let s' = pour scans
  print $ countWater2 s'

img :: String -> IO ()
img s = do
  scans <- getInput' s
  let s' = pour scans

  writePng "day17.png" (mkImg s')

  where
    mkImg s'@(Scan m) = generateImage (mkpixel m) w h

      where
        ((mnx,mny),(mxx,mxy)) = bounds s'
        w = mxx - mnx
        h = mxy - mny

        mkpixel m' x y = c (Map.lookup (x+mnx, y+mny) m')

        c (Just '#') = PixelRGB8 152 105 96
        c (Just '~') = PixelRGB8 0 0 128
        c (Just '|') = PixelRGB8 0 0 255
        c _          = PixelRGB8 194 178 128
