module Sudoku where

import qualified Prelude
import Protolude
import Data.Foldable (minimumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
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

-- This works by first attempting to solve the board entirely through constraint
-- propagation. In the event that does not work, it chooses the unfilled cell with the
-- smallest number of valid choices and attempts to solve after selecting each choice.
-- This is a DFS after making a choice.
solvePuzzle ::
    Board
    -> Maybe Board
solvePuzzle board
    | solved board = Just board
    | not (noContradicton board) = Nothing
    | any null choices = Nothing
    | any ((> 1) . length) choices = search
    | otherwise = solvePuzzle $ choicesToBoard choices
    where
        choices = toChoices board
        search = let
            smallestPendingCell = minimumBy (comparing (length . snd)) . V.filter ((> 1) . length . snd) $ V.indexed choices
            branches = (choices V.//) . (:[]) . (fst smallestPendingCell,) . (:[]) <$> snd smallestPendingCell
            in head . catMaybes $ solvePuzzle . choicesToBoard  <$> branches

-- validate some property of the entire board
runCheck ::
    (V.Vector Int -> Bool)
    -> Board
    -> Bool
runCheck pred board = let
    rows = fmap getValues $ getRow . RowNum <$> [0..8]
    columns = fmap getValues $ getCol . ColNum <$> [0..8]
    boxes = fmap getValues $ getBox . BoxNum <$> [0..8]
    in all pred $ rows <> boxes <> columns
    where
        getValues :: V.Vector Int -> V.Vector Int
        getValues = fmap (board V.!)

solved ::
    Board
    -> Bool
solved = runCheck ((== [1..9]) . sort . V.toList)

-- Ensures there are no duplicates
noContradicton ::
    Board
    -> Bool
noContradicton = runCheck (not . any ((> 1) . length) . group . sort . filter (> 0) . V.toList)


-- Determines the available values for each index. This
-- automatically solves all single-choice empty cells
toChoices ::
    Board
    -> V.Vector [Int]
toChoices board =
    indexChoices <$> V.indexed board
    where
        indexChoices (idx, v)
            | v == 0 = let
                knownValues = colValues idx <> rowValues idx <> boxValues idx
                in [x | x <- [1..9], x `notElem` knownValues]
            | otherwise =  [v]
        rowValues idx = (board V.!) <$> (getRow . RowNum $ idx `div` 9)
        colValues idx = (board V.!)<$> ( getCol . ColNum $ idx `mod` 9 )
        boxValues idx = (board V.!) <$> ( fromJust . find (idx `elem`) $ getBox . BoxNum <$> [0..8] )

choicesToBoard ::
    V.Vector [Int]
    -> Board
choicesToBoard = fmap toN
    where
        toN [x] = x
        toN xs = 0

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


-- https://e-olio.com/wp-content/uploads/2012/10/sudoku025done.png
--
-- possible solutions: 14909066637295736586240000000, a 29 digt number
-- Assume an option can be checked in .01 seconds
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

-- This is a hard puzzle that requires making a guess as to which
-- choice is the correct one
testPuzzle2 :: (Board, Board)
testPuzzle2 =(
      V.fromList [
        4,0,0,0,0,0,8,0,5,
        0,3,0,0,0,0,0,0,0,
        0,0,0,7,0,0,0,0,0,
        0,2,0,0,0,0,0,6,0,
        0,0,0,0,8,0,4,0,0,
        0,0,0,0,1,0,0,0,0,
        0,0,0,6,0,3,0,7,0,
        5,0,0,2,0,0,0,0,0,
        1,0,4,0,0,0,0,0,0]
    , V.fromList [
        4,1,7,3,6,9,8,2,5,
        6,3,2,1,5,8,9,4,7,
        9,5,8,7,2,4,3,1,6,
        8,2,5,4,3,7,1,6,9,
        7,9,1,5,8,6,4,3,2,
        3,4,6,9,1,2,7,5,8,
        2,8,9,6,4,3,5,7,1,
        5,7,3,2,9,1,6,8,4,
        1,6,4,8,7,5,2,9,3
    ]
    )

testPuzzle3 :: Board
testPuzzle3 = V.fromList [
    0,0,0,0,6,0,0,8,0
    ,0,2,0,0,0,0,0,0,0
    ,0,0,1,0,0,0,0,0,0
    ,0,7,0,0,0,0,1,0,2
    ,5,0,0,0,3,0,0,0,0
    ,0,0,0,0,0,0,4,0,0
    ,0,0,4,2,0,1,0,0,0
    ,3,0,0,7,0,0,6,0,0
    ,0,0,0,0,0,0,0,5,0]
