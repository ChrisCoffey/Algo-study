module NQueens (
    Board1(..),
    nQueens1,
    isValidBoard1,
    Board2(..),
    nQueens2,
    isValidBoard2,

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
    rotate90L,
    rotate90R,
    reverseRow,
    reverseCol,
    allDiagonals
) where

import qualified Prelude
import Output (PrettyPrinter, prettyPrint)
import Indexed

import qualified Data.Vector as V
import Data.Matrix (Matrix, matrix, prettyMatrix, getRow, getDiag, getCol, setElem, (!), getElem, nrows, ncols)
import qualified Data.Matrix as MA
import qualified Data.Text as T
import Numeric.Natural
import Protolude hiding (get, set)

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


