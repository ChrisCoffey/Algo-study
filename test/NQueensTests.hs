module NQueensTests (
    tests
) where

import NQueens
import Indexed
import Output

import Protolude hiding (get, set)
import Test.QuickCheck.Instances.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "N Queens" [
    outputTests,
    helperTests
    ]

outputTests :: TestTree
outputTests = testGroup "output" [
    testCase "Empty list prints nothing" $ do
        let output = prettyPrint $ makeBoard1 0
        output @?= ""
    ,
    testCase "Prints an empty board properly" $ do
        let output = prettyPrint $ makeBoard1 8
        output @?= "        \n        \n        \n        \n        \n        \n        \n        "
    ]

helperTests :: TestTree
helperTests = testGroup "helpers" [
    testProperty "getMatrixValue: Pulls proper value from a matrix" $
        \(IM ls, point@(x,y)) ->
            cover 20 (y > lengthN ls) "Y index too large" $
            cover 20 (x > lengthN ls) "X index too large" $
            cover 5 (x == lengthN ls && y == lengthN ls ) "On boundary" $
            let v = getMatrixValue ls point
            in if x < lengthN ls && y < lengthN ls
               then v == ((`get` x) =<< get ls y)
               else isNothing v
    ,
    testCase "extractDiagonal: properly pulls from matrix" $
        extractDiagonal 0 smallMatrix @?= [1,2,3]
    ,
    testProperty "extractDiagonal: respects boundary conditions" $
        \(IM ls, x) ->
            cover 20 (x > lengthN ls) "starting X index too large" $
            cover 5 (x == lengthN ls) "On boundary" $
            let v = extractDiagonal x ls
            in if x < lengthN ls
               then not $ null v
               else null v
    ]
    where
        lengthN = fromIntegral . length
        smallMatrix = [[1,2,3],
                       [0,2,5],
                       [8,4,3]]


newtype IntMatrix = IM [[Int]]
    deriving Show

instance Arbitrary IntMatrix where
    arbitrary = do
        size <- arbitrary
        IM <$> vectorOf size (vectorOf size arbitrary)

