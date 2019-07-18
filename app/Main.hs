module Main where

import Protolude
import NQueens
import Sudoku


main :: IO ()
main =
    putStr . (show :: Int -> Text) . length $ nQueens4 20
