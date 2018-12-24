module Day24Tests where

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Day24

testDamage :: Assertion
testDamage = do
  (Right armies) <- getInput' "input/day24.sample"
  let [g989] = filter (\Army{_initiative=i} -> i == 3) armies  -- immune
      [g4485] = filter (\Army{_initiative=i} -> i == 4) armies -- infection

  -- Infection group 2 attacks defending group 2, killing 84 units
  let dmg = damageEstimate g4485 g989
  assertEqual "est" 107640 dmg
  let Army{_units=u} = takeDamage g989 dmg
  assertEqual "inf g2 vs. imm g2" (989-84) u

testPart1 :: Assertion
testPart1 = do
  (Right a) <-  getInput
  assertEqual "" 14854 (part1' a)

testPart2 :: Assertion
testPart2 = do
  (Right a) <-  getInput
  assertEqual "" 3467 (part2' a)


tests :: [TestTree]
tests = [
  testCase "damage" testDamage,
  testCase "part 1" testPart1,
  testCase "part 2" testPart2
  ]
