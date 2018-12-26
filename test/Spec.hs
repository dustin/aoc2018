{-# LANGUAGE OverloadedStrings #-}

import           Test.Tasty

import qualified ElfcodeTests

import qualified Day1Tests
import qualified Day10Tests
import qualified Day11Tests
import qualified Day15Tests
import qualified Day16Tests
import qualified Day17Tests
import qualified Day18Tests
import qualified Day19Tests
import qualified Day20Tests
import qualified Day21Tests
import qualified Day22Tests
import qualified Day23Tests
import qualified Day24Tests
import qualified Day25Tests
import qualified SearchTests

tests :: [TestTree]
tests = [
  testGroup "day 1" Day1Tests.tests,
  testGroup "day 10" Day10Tests.tests,
  testGroup "day 11" Day11Tests.tests,
  testGroup "search" SearchTests.tests,
  testGroup "day 15" Day15Tests.tests,
  testGroup "elfcode" ElfcodeTests.tests,
  testGroup "day 16" Day16Tests.tests,
  testGroup "day 17" Day17Tests.tests,
  testGroup "day 18" Day18Tests.tests,
  testGroup "day 19" Day19Tests.tests,
  testGroup "day 20" Day20Tests.tests,
  testGroup "day 21" Day21Tests.tests,
  testGroup "day 22" Day22Tests.tests,
  testGroup "day 23" Day23Tests.tests,
  testGroup "day 24" Day24Tests.tests,
  testGroup "day 25" Day25Tests.tests
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
