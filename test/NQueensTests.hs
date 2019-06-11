module NQueensTests (
    tests
) where

import Test.Tasty (tests, testGroup)
import Test.Tasty.Quickcheck (testProperty)

tests :: TestTree
tests = testGroup "N Queens" [
    testProper
    ]

outputTests :: TestTree
outputTests
