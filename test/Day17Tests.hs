module Day17Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day17

run17 :: FilePath -> String -> IO ()
run17 inf outf = do
  s <- Day17.getInput' inf
  let s' = Day17.pour s
  writeFile outf (show s' <> "\n" <> show (Day17.countWater s', Day17.countWater2 s'))

tests :: [TestTree]
tests = [
  goldenVsFileDiff "day17 (flood)" (\ref new -> ["diff", "-qw", ref, new])
    "test/output/day17.txt" "test/output/,day17.txt" (run17 "input/day17" "test/output/,day17.txt")
  ]
