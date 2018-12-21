{-# LANGUAGE OverloadedStrings #-}

module ElfcodeTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import qualified Data.Attoparsec.Text as A
import Data.Text (pack)

import           Elfcode

getInput :: IO (Either String Program)
getInput = A.parseOnly parseProg . pack <$> readFile "input/day21.orig"

-- Verify we don't damage the input with our annotating Show
testParserTransformation :: Assertion
testParserTransformation = do
  (Right p) <- getInput
  assertEqual "" (Right p) (A.parseOnly parseProg . pack . show $ p)

tests :: [TestTree]
tests = [
        testCase "parse/show/parse" testParserTransformation
        ]
