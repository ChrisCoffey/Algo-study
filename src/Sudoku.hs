module Sudoku where

import Protolude
import Data.Maybe (fromJust)
import qualified Data.Vector as V

type Board = V.Vector Int
type Box = V.Vector Int
type Row = V.Vector Int
type Column = V.Vector Int

-- Represents a box on the board, moving L -> R
newtype BoxNum = BoxNum Int
newtype ColNum = ColNum Int
newtype RowNum = RowNum Int

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
    boxOffsets = V.fromList [0, 3, 6, 27, 30, 33, 54]

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

-- Determines the available values for each index
toChoices ::
    Board
    -> V.Vector [Int]
toChoices board =
    indexChoices <$> V.indexed board
    where
        indexChoices (idx, v) = undefined

        missingRowValues idx = let
            row = getRow . RowNum $ idx `div` 9
            in filter (not . (`V.elem` row)) [1..9]
        missingColValues idx = let
            col = getCol . ColNum $ idx `mod` 9
            in filter (not . (`V.elem` col)) [1..9]
        missingBoxValues idx = let
            box = fromJust . find (idx `elem`) $ getBox . BoxNum <$> [0..8]
            in filter (not . (`V.elem` box)) [1..9]
