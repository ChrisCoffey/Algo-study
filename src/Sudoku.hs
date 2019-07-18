module Sudoku where

import qualified Prelude
import Protolude
import Data.Maybe (fromJust)
import qualified Data.Vector as V


-- Uses 0 to mark unknown cells
type Board = V.Vector Int
type Box = V.Vector Int
type Row = V.Vector Int
type Column = V.Vector Int

-- Represents a box on the board, moving L -> R
newtype BoxNum = BoxNum Int
newtype ColNum = ColNum Int
newtype RowNum = RowNum Int

-- Solves a Sudoku puzzle by reducing the initial choices, then generating all
-- possible permutations from the grid, then selecting out the valid one.
solvePuzzle ::
    Board
    -> Board
solvePuzzle board = let
    -- convert the puzzle to the possible choices
    -- keep all cells with a single choice as-is
    -- Algorithm operates on the choices themselves
    possibleSolutions = choicePermutations . V.toList $ toChoices board
    in fromJust . find solved $ V.fromList <$> possibleSolutions
    where
        choicePermutations :: [[Int]] -> [[Int]]
        choicePermutations [] = []
        choicePermutations ([x]:rest) = (x:) <$> choicePermutations rest
        choicePermutations (xs:rest) = let
            remainder = choicePermutations rest
            perms = (\x -> (x:) <$> remainder) <$> xs
            in concat perms

solved ::
    Board
    -> Bool
solved board = let
    rows = fmap getValues $ getRow . RowNum <$> [0..8]
    columns = fmap getValues $ getCol . ColNum <$> [0..8]
    boxes = fmap getValues $ getBox . BoxNum <$> [0..8]
    in all correct $ rows <> boxes <> columns
    where
        correct = (== [1..9]) . sort . V.toList
        getValues :: V.Vector Int -> V.Vector Int
        getValues = fmap (\idx -> board V.! idx)

-- Helper for pulling a specific box from the baord
getBox ::
    BoxNum
    -> Box
getBox (BoxNum bNum) =
    (+ (boxOffsets V.! fromIntegral bNum)) <$> seedBox
    where
    -- This is the reference box, i.e. box0. All other box indices are
    -- computed based off the values here.
    seedBox = V.fromList [0,1,2,
                        9,10,11,
                        18,19,20
                        ]
    -- The offsets for each box, from 0.
    boxOffsets = V.fromList [0, 3, 6, 27, 30, 33, 54, 57, 60]

-- Helper for pulling a specific row from the board
getRow ::
    RowNum
    -> Row
getRow (RowNum rNum) =
    (+ (rNum*9)) <$> seedRow
    where
        seedRow = V.fromList [0..8]

-- Helper for pulling a specific column from the board
getCol ::
    ColNum
    -> Row
getCol (ColNum cNum) =
    (+ cNum) <$> seedCol
    where
        seedCol = V.fromList [0,9..72]

-- Determines the available values for each index. This a
-- automatically solves all single-choice empty cells
toChoices ::
    Board
    -> V.Vector [Int]
toChoices board =
    indexChoices <$> V.indexed board
    where
        indexChoices (idx, v)
            | v == 0 = missingColValues (trace ("choice: " <> show idx :: Text) idx) <> missingRowValues idx <> missingBoxValues idx
            | otherwise =  [v]

        missingRowValues idx = let
            row = getRow . RowNum $ idx `div` 9
            in filter (not . (`V.elem` row)) [1..9]
        missingColValues idx = let
            col = getCol . ColNum $ idx `mod` 9
            in filter (not . (`V.elem` col)) [1..9]
        missingBoxValues idx = let
            box = fromJust . find (idx `elem`) $ getBox . BoxNum <$> [0..8]
            in filter (not . (`V.elem` box)) [1..9]

-- https://e-olio.com/wp-content/uploads/2012/10/sudoku025done.png
testPuzzle :: (Board, Board)
testPuzzle = (V.fromList [0,0,0,0,0,9,8,0,0 -- input
                         ,0,1,8,4,0,0,0,2,0
                         ,0,0,4,0,7,0,0,0,0
                         ,0,0,0,0,0,6,0,0,0
                         ,6,0,0,0,0,0,0,5,0
                         ,0,0,0,1,8,0,7,0,2
                         ,0,5,1,8,0,0,0,9,3
                         ,9,7,0,0,3,0,0,0,4
                         ,0,3,0,0,6,0,0,0,0]
             ,V.fromList [5,6,3,2,1,9,8,4,7 -- solved
                         ,7,1,8,4,5,3,9,2,6
                         ,2,9,4,6,7,8,3,1,5
                         ,1,2,5,7,9,6,4,3,8
                         ,6,8,7,3,4,2,1,5,9
                         ,3,4,9,1,8,5,7,6,2
                         ,4,5,1,8,2,7,6,9,3
                         ,9,7,6,5,3,1,2,8,4
                         ,8,3,2,9,6,4,5,7,1]
            )
