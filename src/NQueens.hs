module NQueens (
    Board1(..),
    nQueens1,
    isValidBoard1,
    Board2(..),
    nQueens2,
    isValidBoard2,
    SparseMatrix(..),
    SparseRows(..),
    SparseCols(..),
    SparseDiags(..),
    emptyBoard3,
    nQueens3,
    isValidBoard3,
    nQueens4,
    computeValid4,

    -- board stuff
    makeBoard1,
    emptyBoard2,
    possibleRows,

    -- Naive matrix stuff
    extractDiagonal,
    getMatrixValue,
    rotate90Counter,
    toBoard1,

    -- Missing Data.Matrix stuff
    X(..),
    Y(..),
    rotate90L,
    rotate90R,
    reverseRow,
    reverseCol,
    allDiagonals,
    perms,
    sparsePerms,
    sparseRowPerms,
    allSparseCols,
    allSparseRows,
    allSparseDiags
) where

import qualified Prelude
import Output (PrettyPrinter, prettyPrint)
import Indexed

import qualified Data.Vector as V
import qualified Data.HashSet as S
import Data.Hashable
import GHC.Generics
import Data.Matrix (Matrix, matrix, prettyMatrix, getRow, getDiag, getCol, setElem, (!), getElem, nrows, ncols)
import qualified Data.Matrix as MA
import qualified Data.Map as M
import qualified Data.Text as T
import Numeric.Natural
import Protolude hiding (get, set)
import qualified Protolude as PR



--
-- Naive algorithm & naive data structures
--

newtype Board1 = B1 [[Bool]]
    deriving (Generic, NFData, Eq)

makeBoard1 ::
    Natural
    -> Board1
makeBoard1 n = B1 $ replicateN n (replicateN n False)

instance PrettyPrinter Board1 where
    prettyPrint (B1 rows) = let
        board = T.unlines $ map rowToLine rows
        in board `T.snoc` '\n'
        where
            rowToLine = T.pack . map toCell
            toCell True = 'Q'
            toCell False = '.'

-- | Check if the board is valid by checking that each row, column, and diagonal contains only a single queen
isValidBoard1 ::
    Board1
    -> Bool
isValidBoard1 (B1 rows) = let
    allRowsValid = all identity $ map checkRow rows
    allColumnsValid = all identity . map checkRow $ transpose rows
    allDiagonalsValid = let
        r1 = rotate90Counter rows
        r2 = rotate90Counter r1
        r3 = rotate90Counter r2
        in checkDiags rows && checkDiags r1 && checkDiags r2 && checkDiags r3
    in allRowsValid && allColumnsValid && allDiagonalsValid
    where
        checkRow :: [Bool] -> Bool
        checkRow = (<= 1) . length . filter identity
        checkDiags :: [[Bool]] -> Bool
        checkDiags rs = all checkRow $ map ( `extractDiagonal` rs) [0.. fromIntegral (length rs) -1]

nQueens1 ::
    Natural
    -> [Board1]
nQueens1 n = let
    allPerms = permutations $ possibleRows n (replicateN n False)
    asBoards = map B1 allPerms
    in filter isValidBoard1 asBoards

-- | All possible permutations for a given row of length n
-- This considers that only a single queen may be placed on each row
possibleRows ::
    Natural
    -> [Bool]
    -> [[Bool]]
possibleRows n emptyRow =
    map (\idx -> fromMaybe [] $ set emptyRow idx True) [0..n -1]

-- | Rotate a matrix 90 degrees counter-clockwise. Think about what this does.
rotate90Counter ::
    [[a]]
    -> [[a]]
rotate90Counter = reverse . transpose

replicateN :: Integral n =>
    n
    -> a
    -> [a]
replicateN n = replicate (fromIntegral n)

-- | Starting from the given x coordinate, extract the diagonal from the matrix. This assumes that
-- it is a rectangular matrix, not a jagged array. The matrix is encoded in the typical fashion, columns in the
-- inner lists, rows on the outer
extractDiagonal ::
    Natural
    -> [[a]]
    -> [a]
extractDiagonal startingXCoord grid
    | null grid = []
    | fromIntegral startingXCoord >= length (headDef [] grid) = []
    | otherwise = let
        maxY = fromIntegral $ max ( length grid ) (fromIntegral startingXCoord)
        indices = [startingXCoord..maxY] `zip` [0..maxY]
        in mapMaybe (getMatrixValue grid) indices

-- | Extract a single value from a matrix in list representation
getMatrixValue ::
    [[a]]
    -> (Natural, Natural)
    -> Maybe a
getMatrixValue matrix point = do
    row <- matrix `get` snd point
    row `get` fst point


--
-- Part 2:
--
--
-- Naive algorithm, smarter data structure
--


newtype Board2 = B2 (Matrix Bool)
    deriving (Generic, NFData)

toBoard1 ::
    Board2
    -> Board1
toBoard1 (B2 matrix) = B1 $ MA.toLists matrix

emptyBoard2 ::
    Natural
    -> Board2
emptyBoard2 n = let
    n' = fromIntegral n
    in B2 $ matrix n' n'  (const False)

instance PrettyPrinter Board2 where
    prettyPrint (B2 xs) = ((`T.snoc` '\n') . T.pack . prettyMatrix) $ transform <$> xs
        where
            transform True = 'Q'
            transform False = '.'

isValidBoard2 ::
    Board2
    -> Bool
isValidBoard2 (B2 board) = let
    allRowsValid = checkRows $ (`getRow` board) <$> [1..size]
    allColsValid = checkRows $ (`getCol` board) <$> [1..size]
    validDiagonals = let
        r1 = rotate90L board
        in allDiagonalsValid board && allDiagonalsValid r1
    in allRowsValid && allColsValid && validDiagonals
    where
        size = MA.nrows board
        checkRows = all (<=1) . fmap (V.length . V.filter identity)
        allDiagonalsValid :: Matrix Bool -> Bool
        allDiagonalsValid = checkRows . allDiagonals


-- | Extract all of the diagonals through a square matrix
allDiagonals ::
    Matrix a
    -> [V.Vector a]
allDiagonals matrix = xDiagonals<>yDiagonals
    where
        size = MA.nrows matrix
        indices = [1..size]
        xDiagonals = getDiag . (\n -> MA.submatrix 1 (size - (n-1)) n size matrix) <$> indices
        yDiagonals = getDiag . (\n -> MA.submatrix n size 1 (size - (n-1)) matrix) <$> indices


nQueens2 ::
    Natural
    -> [Board2]
nQueens2 n =
    filter isValidBoard2 boards
    where
        boards = fmap (B2 . MA.fromLists) . permutations $ possibleRows n (replicateN n False)

-- | Rotate a matrix 90 degrees
rotate90L ::
    Matrix a
    -> Matrix a
rotate90L matrix =
    foldr reverseCol (MA.transpose matrix) (fromIntegral <$> [1..ncols matrix])

rotate90R ::
    Matrix a
    -> Matrix a
rotate90R matrix =
    foldr reverseRow (MA.transpose matrix) (fromIntegral <$> [1..ncols matrix])

-- | Reverse the elements in a given row w/in the matrix
reverseRow ::
    Natural
    -> Matrix a
    -> Matrix a
reverseRow i matrix
    | nrows matrix < iInt = Prelude.error "Out of bounds"
    | otherwise = foldr swap matrix [1..cols `div` 2]
    where
        iInt = fromIntegral i
        cols = ncols matrix
        swap j mx = let
            x = mx ! (iInt, j)
            mx' = setElem (mx ! (iInt, cols - (j-1))) (iInt, j) mx
            in setElem x (iInt, cols - (j-1)) mx'

-- Reverse the elements in a given column within the matrix
reverseCol ::
    Natural
    -> Matrix a
    -> Matrix a
reverseCol j matrix
    | ncols matrix < jInt = Prelude.error "Out of bounds"
    | otherwise = foldr swap matrix [1..rows `div` 2]
    where
        jInt = fromIntegral j
        rows = nrows matrix
        swap i mx = let
            x = mx ! (i, jInt)
            mx' = setElem (mx ! (rows - (i-1), jInt)) (i, jInt) mx
            in setElem x (rows - (i-1), jInt) mx'

--
-- Part 3:
--
--
-- Naive algorithm, but even more intelligent data structure
--

newtype X = X {unX :: Natural}
    deriving stock (Generic)
    deriving newtype (Eq, Ord, Show, Hashable, Num, Enum)

newtype Y = Y {unY :: Natural}
    deriving stock (Generic)
    deriving newtype (Eq, Ord, Show, Hashable, Num, Enum)

data SparseMatrix = SparseMatrix {
    numRows :: Natural
    ,numCols :: Natural
    ,values :: S.HashSet (X, Y)
    }
    deriving (Show, Eq)

instance PrettyPrinter SparseMatrix where
    prettyPrint SparseMatrix {values, numRows, numCols} =
        foldl' f "" [ (X x, Y y)| x <- [0..numCols -1],
                                        y <- [0..numRows -1]]
        where
            f output pt@(X x, Y y) = let
                c = if pt `S.member` values then 'Q' else '.'
                in if numRows `rem` x  == 0
                   then output `T.snoc` c `T.snoc` '\n'
                   else output `T.snoc` c

emptyBoard3 ::
    Natural
    -> SparseMatrix
emptyBoard3 n = SparseMatrix {
     numRows = n
    ,numCols = n
    ,values = S.empty
    }


nQueens3 ::
    Natural
    -> [SparseMatrix]
nQueens3 n =
    filter (isValidBoard3 rows cols diags) $ sparsePerms n
    where
        rows = allSparseRows n
        cols = allSparseCols n
        diags = allSparseDiags n

isValidBoard3 ::
    SparseRows
    -> SparseCols
    -> SparseDiags
    -> SparseMatrix
    -> Bool
isValidBoard3 (SR rows) (SC cols) (SD diags) (SparseMatrix {values}) = let
    rowsValid = check rows
    colsValid = check cols
    diagsValid = check diags
    in rowsValid && colsValid && diagsValid
    where
        check = all ((<= 1) . length) . map (filter (`S.member` values))

newtype SparseRows = SR {unSR::[[(X,Y)]]}
    deriving Show
newtype SparseCols= SC {unSC:: [[(X,Y)]]}
    deriving Show
newtype SparseDiags= SD {unSD:: [[(X,Y)]]}
    deriving Show
-- use lookup tables to detmerine which whether each row, column, and diagonal
-- is safe
allSparseRows ::
    Natural
    -> SparseRows
allSparseRows 1 =
    SR [[(X 0, Y 0)]]
allSparseRows n =
    SR $ sparseRowPerms n <$> [0..n-1]

allSparseCols ::
    Natural
    -> SparseCols
allSparseCols 1 =
    SC [[(X 0, Y 0)]]
allSparseCols n =
    SC $ (\x -> (X x,) . Y <$> [0..n-1]) <$> [0..n-1]

allSparseDiags ::
    Natural
    -> SparseDiags
allSparseDiags 1 =
    SD [[(X 0, Y 0)]]
allSparseDiags n = let
    rDescDiags =  map (map toPoint) $ map (\x -> [x..size] `zip` [0..size-x]) [0..size]
    lDescDiags = map (map toPoint) $ map (\y -> [0..size-y] `zip` [y..size]) [0..size]
    rAscDiags = map (map toPoint) $ map (\x -> [x..size] `zip` [size,size-1..0]) [0..size]
    lAscDiags = map (map toPoint) $ map (\y -> [0..size-y] `zip` (reverse [0..size-y])) [0..size]
    in SD $ rAscDiags<>lAscDiags<>rDescDiags<>lDescDiags
    where
        size = n-1
        toPoint (x,y) = (X x, Y y)

sparseRowPerms ::
    Natural
    -> Natural
    -> [(X, Y)]
sparseRowPerms col row =
    [(X x, Y row) | x <- [0..col -1]]

sparsePerms ::
    Natural
    -> [SparseMatrix]
sparsePerms n =
    toSparseMatrix <$> perms rowPossibilities
    where
        rowPossibilities :: [[(X,Y)]]
        rowPossibilities = sparseRowPerms n <$> [0..n-1]
        toSparseMatrix :: [(X,Y)] -> SparseMatrix
        toSparseMatrix px = SparseMatrix {
            numRows = n,
            numCols = n,
            values = S.fromList px
            }

-- given [[a]], detemrine all possible [a] made up of one element from
-- each row
-- so, given [[1], [2]], the result is [1,2]
-- given [[1,2],[3,4]], the result is [[1,3], [1,4], [2,3], [2,4]]
perms ::
    [[a]]
    -> [[a]]
-- The next two are the base case
perms [rest] = (:[]) <$> rest
perms ([x]:rest) = (x:) <$> perms rest
-- This is the inductive step
perms ((x:xs):rest) =
    ((x:) <$> perms rest) <> perms (xs:rest)

-- Version 4:
--
-- Sparse matrix with backtracking algorithm
--
--

nQueens4 ::
    Natural
    -> [SparseMatrix]
nQueens4 = computeValid4


newtype Depth = Depth Natural

computeValid4 ::
    Natural
    -> [SparseMatrix]
computeValid4 n =
    evalState (step (Depth $ n -1)) allPoints
    where
        allPoints = S.fromList [(X x, Y y) | x <- [0..n-1], y <- [0..n-1]]

        step :: Depth -> State (S.HashSet (X,Y)) [SparseMatrix]
        -- The base case, when there are no more branches to take. At this point,
        -- if there are any points remaining in the "available point" set, then these are all
        -- valid and should be added to the board
        step (Depth 0) = do
            remainingPoints <- S.toList <$> PR.get
            let toMatrix x = SparseMatrix {
                 numRows = n
                ,numCols = n
                ,values = S.fromList [x]
                }
            pure $ toMatrix <$> remainingPoints

        -- The inductive case. Compute each valid point on the row, then explore down the
        -- tree of possibilities for that point. Each time the algo explores deeper down the tree,
        -- it marks off all points invalidated by the chosen point
        step (Depth d) = do
            availablePoints <- PR.get
            let possibleRowPoints = sparseRowPerms n d
                validRowPoints = filter (`S.member` availablePoints) possibleRowPoints
            results <- traverse explore validRowPoints
            pure $ concat results
            where
                -- For a valid point:
                --  1) filter out the points invalidated by this point
                --  2) update the state to reflect that
                --  3) recursively compute the next setps based on the invalidated points
                --  4) reset the state back to what it started at
                --  5) Add the valid point to the results of the recursive exploration
                explore :: (X,Y) -> State (S.HashSet (X,Y)) [SparseMatrix]
                explore point = do
                    availablePoints <- PR.get
                    let filteredPoints = invalidatePoints point availablePoints
                    if null filteredPoints
                    then pure []
                    else  do
                        PR.put filteredPoints
                        results <- step (Depth $ d-1)
                        PR.put availablePoints -- This is the reset
                        let addToMatrix m@(SparseMatrix {values}) = m {values = S.insert point values}
                        pure $ addToMatrix <$> results
                    where
                        invalidatePoints :: (X, Y) -> S.HashSet (X,Y) -> S.HashSet (X,Y)
                        invalidatePoints (x, y) = S.filter (\(x', y') -> x' /= x && y' /= y && notOnDiagonal (x', y'))
                            where
                                size = n-1
                                oX = X 1
                                zX = X 0
                                oY = Y 1
                                zY = Y 0
                                notOnDiagonal (x', y') = let
                                    upLeft = if x == zX || y == zY
                                            then []
                                            else [x,x-oX..zX] `zip` [y, y-oY..zY]

                                    upRight = if x == X size || y == zY
                                            then []
                                            else [x..X size] `zip` [y, y-oY..zY]

                                    downLeft = if x == zX || y == (Y size)
                                            then []
                                            else [x, x-oX..zX] `zip` [y.. Y size]

                                    downRight = if x == (X size) || y == (Y size)
                                                then []
                                                else [x..X size] `zip` [y.. Y size]

                                    in (x',y') `notElem` (upLeft <> upRight <> downLeft <> downRight)

