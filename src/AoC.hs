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
  Parser, parseFile
  ) where

import           Data.Text             (Text, pack)
import           Data.Void             (Void)
import           Text.Megaparsec       (Parsec, parse)
import           Text.Megaparsec.Error (errorBundlePretty)

-- | A Megaparsec Parser.
type Parser = Parsec Void Text

-- | Load a file (e.g. "input/day24") with the given parser.
parseFile :: Parser a -> String -> IO a
parseFile f s = do
  c <- pack <$> readFile s
  case parse f s c of
    (Left x)  -> fail (errorBundlePretty x)
    (Right v) -> pure v
