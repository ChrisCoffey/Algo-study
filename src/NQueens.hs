module NQueens (
    Board1
) where

import Output (PrettyPrinter, prettyPrint)

import qualified Data.Text as T
import Numerics.Natural
import Protolude

newtype Board1 = B1 [[Bool]]

makeBoard1 ::
    Natural
    -> Board1
makeBoard1 n = B1 $ replicate n (replicate n False)

instance PrettyPrinter Board1 where
    prettyPrint (B1 rows) = T.intercalate '\n' $ map rowToLine rows
        where
            rowToLine = T.pack . map toCell
            toCell True = 'Q'
            toCell False = ' '

isValidBoard1 ::
    Board1
    -> Bool
isValidBoard1 (B1 rows) = let
    allRowsValid = map checkRow rows
    allColumnsValid = map checkRow $ transpose rows
    allDiagonalsValid = map checkDiags rows && map checkDiags (transpose rows)
    in allRowsValid && allColumnsValid && allDiagonalsValid
    where
        checkRow = length (== 1) . filter id
        checkDiags = map checkDiagonal [0..n] rows &&
                     map checkDiagonal [0..n] (transpose rows)
            where
                checkDiagonal start rows' = let
                    xPoints = map (+ start) [0..(n-start)-1]
                    yPoints = [0..]
                    points = zip xPoints yPoints
                    values = map (\(x,y) -> (rows' !! y) !! x) points
                    in checkRow values


nQueens1 ::
    Natural
    -> [Board1]
nQueens1 n = let
    allBoards =
    in filter isValidBoard1 allBoard
    where
        emptyBoard = makeBoard1 n
        placeQueen row col


