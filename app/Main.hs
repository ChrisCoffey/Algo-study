module Main where

import Protolude
import NQueens


main :: IO ()
main =
    putStr . (show :: Int -> Text) . length $ nQueens4 20
