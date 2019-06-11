module NQueens (
    Board1(..),

    -- board stuff
    makeBoard1,

    -- Naive matrix stuff
    extractDiagonal,
    getMatrixValue
) where

import Output (PrettyPrinter, prettyPrint)
import Indexed

import qualified Data.Text as T
import Numeric.Natural
import Protolude hiding (get, set)

newtype Board1 = B1 [[Bool]]

makeBoard1 ::
    Natural
    -> Board1
makeBoard1 n = B1 $ replicateN n (replicateN n False)

instance PrettyPrinter Board1 where
    prettyPrint (B1 rows) = T.intercalate "\n" $ map rowToLine rows
        where
            rowToLine = T.pack . map toCell
            toCell True = 'Q'
            toCell False = ' '

isValidBoard1 ::
    Board1
    -> Bool
isValidBoard1 (B1 rows) = let
    allRowsValid = all identity $ map checkRow rows
    allColumnsValid = all identity . map checkRow $ transpose rows
    allDiagonalsValid = checkDiags rows && checkDiags (transpose rows)
    in allRowsValid && allColumnsValid && allDiagonalsValid
    where
        checkRow :: [Bool] -> Bool
        checkRow = (== 1) . length . filter identity
        checkDiags :: [[Bool]] -> Bool
        checkDiags = undefined

nQueens1 ::
    Natural
    -> [Board1]
nQueens1 n = let
    allBoards = undefined
    in filter isValidBoard1 allBoards
    where
        emptyBoard = makeBoard1 n


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
