module Main where

import Protolude
import NQueens
import Sudoku


main :: IO ()
main =
    putStr . (show :: Maybe Board -> Text) . solvePuzzle $ fst testPuzzle
