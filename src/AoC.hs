{-|
Module      : AoC
Description : Common stuff for AoC
Copyright   : (c) Dustin Sallings, 2018
License     : MIT
Maintainer  : dustin@spy.net
Stability   : experimental
Portability : POSIX

Common stuff across AoC solutions.
-}
module AoC (
  Parser, parseFile,
  mdist2, mdist3, mdist4,
  zipt2, zipt3, zipt4
  ) where

import           Data.Text             (Text, pack)
import           Data.Void             (Void)
import           Text.Megaparsec       (Parsec, parse)
import           Text.Megaparsec.Error (errorBundlePretty)

-- | A Megaparsec Parser.
type Parser = Parsec Void Text

-- | Load a file (e.g. "input/day24") with the given parser.
parseFile :: Parser a -> String -> IO a
parseFile f s = pack <$> readFile s >>= either (fail.errorBundlePretty) pure . parse f s

-- | Parallel application of a function across elements of a tuple.
zipt2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
zipt2 f (a1,b1) (a2,b2) = (f a1 a2, f b1 b2)

-- | Parallel application of a function across elements of a tuple.
zipt3 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
zipt3 f (a1,b1,c1) (a2,b2,c2) = (f a1 a2, f b1 b2, f c1 c2)

-- | Parallel application of a function across elements of a tuple.
zipt4 :: (a -> b -> c) -> (a,a,a,a) -> (b,b,b,b) -> (c,c,c,c)
zipt4 f (a1,b1,c1,d1) (a2,b2,c2,d2) = (f a1 a2, f b1 b2, f c1 c2, f d1 d2)

-- | Two dimensional manhattan distance.
mdist2 :: (Int,Int) -> (Int,Int) -> Int
mdist2 as bs = let (a,b) = zipt2 mdist1 as bs in a+b

-- | Three dimensional manhattan distance.
mdist3 :: (Int,Int,Int) -> (Int,Int,Int) -> Int
mdist3 as bs = let (a,b,c) = zipt3 mdist1 as bs in a+b+c

-- | Four dimensional manhattan distance.
mdist4 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
mdist4 as bs = let (a,b,c,d) = zipt4 mdist1 as bs in a+b+c+d

-- | One dimensional manhattan distance (for combining with the above).
mdist1 :: Int -> Int -> Int
mdist1 a b = abs (a - b)
