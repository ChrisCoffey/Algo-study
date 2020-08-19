{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Kindergarten where

import Prelude

import Control.Monad
import Data.Maybe
import Debug.Trace
import qualified Data.IntMap.Strict as M
import qualified Data.List
import System.Environment
import System.IO

--
-- Complete the solve function below.
--
solve :: [Int] -> Int
solve times = let
  counts = foldl trackNonFinishers M.empty $ zip times [0..]
  (bestRemaining, _, _) = M.foldlWithKey' findMaximum (1, 0, minBound :: Int) counts
  in bestRemaining
  where
    len = length times

    -- This works by creating an offsetting adjusgment at the end of each range. My initial
    -- implementation marked the entire range, which devloved into O(n^2) pretty quickly. This
    -- approach keeps the complexity at O(n)!
    -- The idea to use a constant number rather than input dependent number of update to `counts`
    -- is excellent. I should keep this pattern in mind for future problems.
    trackNonFinishers counts (t, i)
      | t == len = counts
      | t == 0 = counts
      | otherwise = let
        startRange = (i - t+1+len) `mod` len
        endRange = if i == (len -1) then 0 else (i + 1)
        startMarked = M.insertWith (+) startRange (-1) counts
        in M.insertWith (+) endRange 1 startMarked

    findMaximum (x, sum, maxSum) key value
      | (sum + value) > maxSum = (key +1, sum + value, sum + value)
      | otherwise = (x, sum + value, maxSum)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    tTemp <- readFile "data/kindergarten.txt"

    let t = fmap (read :: String -> Int) . words $ tTemp

    let id = solve t
    print id
