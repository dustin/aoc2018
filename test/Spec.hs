{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests = [
  ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
