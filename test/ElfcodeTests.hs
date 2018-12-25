{-# LANGUAGE OverloadedStrings #-}

module ElfcodeTests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import Data.Text (pack)
import           Text.Megaparsec            (parse)

import           Elfcode
import           AoC                        (parseFile)

getInput :: IO Program
getInput = parseFile parseProg "input/day21.orig"

-- Verify we don't damage the input with our annotating Show
testParserTransformation :: Assertion
testParserTransformation = do
  p <- getInput
  assertEqual "" (Right p) (parse parseProg "" . pack . show $ p)

testAlign :: Assertion
testAlign = do
  let got = align "abc\td\nefghi\tjkl\n"
  let want = "abc   d\nefghi jkl\n"
  assertEqual "" want got

testAlignMissingColumn :: Assertion
testAlignMissingColumn = do
  let got = align "ab\tc\tdef\nab\t\tcd"
  let want = "ab c def\nab   cd\n"
  assertEqual "" want got


tests :: [TestTree]
tests = [
        testCase "parse/show/parse" testParserTransformation,
        testCase "align" testAlign,
        testCase "align missing col" testAlignMissingColumn
        ]
