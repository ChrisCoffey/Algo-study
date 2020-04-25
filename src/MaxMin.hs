module MaxMin where

-- start: Sat Apr 25 15:22:32 EDT 2020
-- end:Sat Apr 25 16:07:34 EDT 2020

import Protolude
import Prelude(String, read)

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

type Arr = Array Int Int
newtype Score = Score Int
newtype K = K Int

-- Complete the maxMin function below.
maxMin :: Int -> [Int] -> Int
maxMin k rawInput = let
    (Score result, _) = walkInput (Score startingScore) (k - 1)
    in result
    where
        sortedInput = sort rawInput
        arr  = listArray (0, length sortedInput -1) sortedInput
        (_, maxBound) = bounds arr
        startingScore = let
            slice = Data.List.take k sortedInput
            fst = Data.List.head slice
            lst = last slice
            in lst - fst

        walkInput score rightHandIndex
            | rightHandIndex == maxBound = (score, rightHandIndex)
            | otherwise = uncurry walkInput $ slide arr score (K k) rightHandIndex

slide ::
    Arr
    -> Score
    -> K
    -> Int -- Current index
    -> (Score, Int)
slide arr (Score s) (K k) currentIndex = let
    nextIndex = currentIndex + 1
    inElem = arr ! nextIndex
    outElem =  arr ! (nextIndex - k +1)
    s' = inElem - outElem
    in if s' < s
       then (Score s', nextIndex)
       else (Score s, nextIndex)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- System.IO.getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    k <- readLn :: IO Int

    arrTemp <- readMultipleLinesAsStringArray n
    let arr = Data.List.map (read :: String -> Int) arrTemp

    let result = maxMin k arr

    System.IO.hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
