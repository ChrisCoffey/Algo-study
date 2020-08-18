{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module HourglassArray where

import Prelude

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import qualified Data.Map as M
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

type Point = (Int, Int)
type Matrix = M.Map Point Int

-- Complete the hourglassSum function below.
hourglassSum :: [[Int]] -> Int
hourglassSum arr = maximum [ hourglass x y |
  x <- [0..maxWidth],
  y <- [0..maxHeight]
  ]
  where
    maxHeight = (length arr) - 3
    maxWidth = (length $ head arr) - 3
    matrix = toMatrix arr
    hourglass x y = sum [
      matrix M.! (x', y') |
      x' <- [x..x+2],
      y' <- [y..y+2],
      (x',y') /= (x, y+1) && (x', y') /= (x+2, y+1)
      ]


toMatrix :: [[Int]] -> Matrix
toMatrix arr =
  M.fromList . concat $ toPoints <$> zipWithIndex arr
  where
    zipWithIndex = (`zip` [0..])
    toPoints :: ([a], Int) -> [(Point, a)]
    toPoints (elems, y) = dataWithPoint y <$> zipWithIndex elems

    dataWithPoint :: Int -> (a, Int) -> (Point, a)
    dataWithPoint y (dta, x) = ((x, y), dta)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    arrTemp <- readMultipleLinesAsStringArray 6
    let arr = Data.List.map (\x -> Data.List.map (read :: String -> Int) . words $ x) arrTemp

    let result = hourglassSum arr

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
