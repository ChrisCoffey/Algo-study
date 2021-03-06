module NQueensTests (
    tests
) where

import NQueens
import Indexed
import Output

import qualified Data.Vector as V
import qualified Data.Matrix as MA
import qualified Data.HashSet as S
import Protolude hiding (get, set)
import Test.QuickCheck.Instances.Natural
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "N Queens" [
    v1Tests,
    v2Tests,
    v3Tests,
    outputTests,
    helperTests,
    sparseMatrixTests
    ]

v1Tests :: TestTree
v1Tests = testGroup "v1 N-Queens" [
    testCase "isValidBoard1 succeeds for trivial case" $ do
        let b = B1 [[True]]
        isValidBoard1 b @?= True
    ,
    testCase "isValidBoard1 rejects row failures" $ do
        let b = B1 [[True, True], [False, False]]
        isValidBoard1 b @?= False
    ,
    testCase "isValidBoard1 rejects column failures" $ do
        let b = B1 [[True, False], [True, False]]
        isValidBoard1 b @?= False
    ,
    testCase "isValidBoard1 rejects diagonal failures" $ do
        let b = B1 [[True, False], [False, True]]
        isValidBoard1 b @?= False
    ,
    testCase "isValidBoard1 succeeds for moderate case" $ do
        let b = B1 [
                 [False,True,False,False]
                ,[False,False,False,True]
                ,[True,False,False,False]
                ,[False,False,True,False]
                ]
        isValidBoard1 b @?= True
    ]

v2Tests :: TestTree
v2Tests = testGroup "v2 N-Queens" [
    testCase "isValidBoard2 succeeds for trivial case" $ do
        let b = B2 $ MA.fromLists [[True]]
        isValidBoard2 b @?= True
    ,
    testCase "isValidBoard2 rejects row failures" $ do
        let b = B2 $ MA.fromLists [[True, True], [False, False]]
        isValidBoard2 b @?= False
    ,
    testCase "isValidBoard2 rejects column failures" $ do
        let b = B2 $ MA.fromLists [[True, False], [True, False]]
        isValidBoard2 b @?= False
    ,
    testCase "isValidBoard2 rejects diagonal failures" $ do
        let b = B2 $ MA.fromLists [[True, False], [False, True]]
        isValidBoard2 b @?= False
    ,
    testCase "isValidBoard2 rejects all 2x2 boards" $ do
        let b = B2 $ MA.fromLists [[True, False],
                                   [False, True]]
            c = B2 $ MA.fromLists [[True, True],
                                   [False, False]]
            d = B2 $ MA.fromLists [[False, True],
                                   [False, True]]
            e = B2 $ MA.fromLists [[False, False],
                                   [True, True]]
            f = B2 $ MA.fromLists [[True, False],
                                   [True, False]]
            g = B2 $ MA.fromLists [[False, True],
                                   [True, False]]
        isValidBoard2 b @?= False
        isValidBoard2 c @?= False
        isValidBoard2 d @?= False
        isValidBoard2 e @?= False
        isValidBoard2 f @?= False
        isValidBoard2 g @?= False
    ,
    testCase "isValidBoard2 succeeds for moderate case" $ do
        let b = B2 $ MA.fromLists [
                 [False,True,False,False]
                ,[False,False,False,True]
                ,[True,False,False,False]
                ,[False,False,True,False]
                ]
        isValidBoard2 b @?= True
    ,
    testCase "isValidBoard2 rejects poor moderate cases" $ do
        let b = B2 $ MA.fromLists [
                 [False,True,False,True]
                ,[False,False,False,False]
                ,[True,False,False,False]
                ,[False,False,True,False]
                ]
            c = B2 $ MA.fromLists [
                 [False,True,False,False]
                ,[False,False,True,False]
                ,[True,False,False,False]
                ,[False,True,False,False]
                ]
            d = B2 $ MA.fromLists [
                 [True,False,False,False]
                ,[False,False,False,True]
                ,[True,False,False,False]
                ,[False,False,True,False]
                ]
            e = B2 $ MA.fromLists [
                 [False,True,False,False]
                ,[False,False,False,True]
                ,[False,True,False,False]
                ,[False,False,True,False]
                ]
            f = B2 $ MA.fromLists [
                 [False,True,False,False]
                ,[False,False,False,True]
                ,[False,False,True,False]
                ,[False,False,True,False]
                ]
        isValidBoard2 b @?= False
        isValidBoard2 c @?= False
        isValidBoard2 d @?= False
        isValidBoard2 e @?= False
        isValidBoard2 f @?= False
    ]

v3Tests :: TestTree
v3Tests = testGroup "V3: Sparse Matrix with naive algo" [
    testCase "isValidBoard3: identifies the trivial board" $ do
        let trivialBoard = [(X 0, Y 0)]
        isValidBoard3 (SR [trivialBoard])
                      (SC [trivialBoard])
                      (SD [trivialBoard])
                      (SparseMatrix 1 1 (S.fromList trivialBoard)) @?= True
    ,
    testCase "isValidBoard3: identifies a 4x4 board" $
        isValidBoard3 rx cx dx fourByFour @?= True
    ,
    testCase "isValidBoard3: rejects invalid boards" $ do
        let
            a = SparseMatrix 4 4 $ S.fromList [
                (X 1, Y 0)
                ,(X 1, Y 1)
                ,(X 0, Y 2)
                ,(X 2, Y 3)]
            b = SparseMatrix 4 4 $ S.fromList [
                (X 0, Y 0)
                ,(X 1, Y 1)
                ,(X 3, Y 2)
                ,(X 2, Y 3)]
            c = SparseMatrix 4 4 $ S.fromList [
                (X 2, Y 0)
                ,(X 3, Y 1)
                ,(X 1, Y 2)
                ,(X 0, Y 3)]
            d = SparseMatrix 4 4 $ S.fromList [
                (X 1, Y 0)
                ,(X 3, Y 0)
                ,(X 0, Y 2)
                ,(X 2, Y 3)]
            e = SparseMatrix 4 4 $ S.fromList [
                (X 2, Y 0)
                ,(X 3, Y 1)
                ,(X 0, Y 2)
                ,(X 1, Y 3)]
        isValidBoard3 rx cx dx a @?= False
        isValidBoard3 rx cx dx b @?= False
        isValidBoard3 rx cx dx c @?= False
        isValidBoard3 rx cx dx d @?= False
        isValidBoard3 rx cx dx e @?= False
    ]
    where
        rx = allSparseRows 4
        cx = allSparseCols 4
        dx = allSparseDiags 4
        fourByFour = SparseMatrix 4 4 $ S.fromList [
                (X 1, Y 0)
                ,(X 3, Y 1)
                ,(X 0, Y 2)
                ,(X 2, Y 3)
            ]


outputTests :: TestTree
outputTests = testGroup "output" [
    testCase "Empty list prints nothing" $ do
        let output = prettyPrint $ makeBoard1 0
        output @?= "\n"
    ,
    testCase "Prints an empty board properly" $ do
        let output = prettyPrint $ makeBoard1 8
        output @?= "........\n........\n........\n........\n........\n........\n........\n........\n\n"
    ,
    testCase "Empty matrix prints nothing" $ do
        let output = prettyPrint $ emptyBoard2 0
        output @?= "\9484  \9488\n\9492  \9496\n"
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
    ,
    testCase "rotate90R: rotates a matrix 90 degrees right" $ let
        m1 = rotate90R $ MA.fromLists smallMatrix
        m2 = rotate90R $ MA.fromLists medMatrix
        in do
            m1 @?= MA.fromLists [[8,0,1],
                                 [4,2,2],
                                 [3,5,3]]
            m2 @?= MA.fromLists [[0,1,0,0,0,0],
                                 [0,0,0,0,1,0],
                                 [1,0,0,0,0,0],
                                 [0,0,0,0,0,1],
                                 [0,0,0,1,0,0],
                                 [0,0,1,0,0,0]]
    ,
    testCase "rotate90L: rotates a matrix 90 degrees left" $ let
        m1 = rotate90L $ MA.fromLists smallMatrix
        m2 = rotate90L $ MA.fromLists medMatrix
        in do
            m1 @?= MA.fromLists [[3,5,3],
                                 [2,2,4],
                                 [1,0,8]]
            m2 @?= MA.fromLists [[0,0,0,1,0,0],
                                 [0,0,1,0,0,0],
                                 [1,0,0,0,0,0],
                                 [0,0,0,0,0,1],
                                 [0,1,0,0,0,0],
                                 [0,0,0,0,1,0]]
    ,
    testCase "allDiagonals: extracts proper diagonals" $ let
        m1 = allDiagonals $ MA.fromLists smallMatrix
        m2 = allDiagonals $ MA.fromLists medMatrix
        in do
            m1 @?= V.fromList <$> [[1,2,3],[2,5],[3],
                                 [1,2,3], [0,4],[8]]
            m2 @?= V.fromList <$> [[0,1,0,0,0,0],[0,0,0,0,0], [0,0,1,1], [1,0,0], [0,0], [0],
                                 [0,1,0,0,0,0],[0,0,0,0,0], [0,0,0,0], [0,0,1], [1,0], [0]]
    ]
    where
        lengthN = fromIntegral . length
        smallMatrix = [[1,2,3],
                       [0,2,5],
                       [8,4,3]]
        medMatrix = [[0,0,0,1,0,0],
                     [0,1,0,0,0,0],
                     [0,0,0,0,1,0],
                     [0,0,0,0,0,1],
                     [1,0,0,0,0,0],
                     [0,0,1,0,0,0]]


sparseMatrixTests :: TestTree
sparseMatrixTests = testGroup "SparseMatrix" [
    testCase "sparseRowPerms generates all possible single-queen rows" $ do
        sparseRowPerms 2 0 @?= [(X 0, Y 0), (X 1, Y 0)]
        sparseRowPerms 4 0 @?= [(X 0, Y 0), (X 1, Y 0),(X 2, Y 0), (X 3, Y 0)]
    ,
    testCase "sparsePerms generates all permutaions of an nxn grid with one Q per row" $ do
        sparsePerms 2 @?= twoByTwo
    ]
    where
        twoByTwo = [
              SparseMatrix {numCols=2, numRows=2, values=S.fromList [
                (X 0, Y 0), (X 0, Y 1)
                ]}
            , SparseMatrix {numCols=2, numRows=2, values=S.fromList [
                (X 0, Y 0), (X 1, Y 1)
                ]}
            , SparseMatrix {numCols=2, numRows=2, values=S.fromList [
                (X 1, Y 0), (X 0, Y 1)
                ]}
            , SparseMatrix {numCols=2, numRows=2, values=S.fromList [
                (X 1, Y 0), (X 1, Y 1)
                ]}
            ]

newtype IntMatrix = IM [[Int]]
    deriving Show

instance Arbitrary IntMatrix where
    arbitrary = do
        size <- arbitrary
        IM <$> vectorOf size (vectorOf size arbitrary)

