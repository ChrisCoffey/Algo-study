module FradulentActivity where

-- start: Thu Apr  9 08:25:29 EDT 2020
-- end: Not working
-- correct but slow at Thu Apr  9 09:40:09 EDT 2020

import Protolude
import Prelude(String, read)

import Control.Monad
import Data.Array.IArray
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

data SlidingWindow =
    SlidingWindow {
        remainingCapacity :: Int,
        insertionOrder :: Days,
        sortedOrder :: [Int]
    }

type Days = Array Int Int

activityNotifications' :: [Int] -> Int -> Int
activityNotifications' expenditures d =
    case lookBack' d expenditures of
        Nothing -> 0
        Just (today, window, rest) -> let
           med = median window
           x = if today >= (2*med) then 1 else 0
           in x + activityNotifications' rest d

lookBack' :: Int -> [Int] -> Maybe (Int, [Int], [Int])
lookBack' len expenditure@(_:rest) =
    case accum expenditure len [] of
        Nothing -> Nothing
        Just (window, day) -> Just (day, window, rest)
    where
        accum [] n res = Nothing
        accum (x:xs) 0 res = Just (res, x)
        accum (x:xs) n res = accum xs (n-1) (insSort x res)

        insSort n [] = [n]
        insSort n (x:rest)
            | n > x = x : insSort n rest
            | otherwise = n:x:rest

-- Complete the activityNotifications function below.
activityNotifications :: [Int] -> Int -> Int
activityNotifications expenditure d = let
    ixs = indices days
    n = Data.List.foldl' accum 0 ixs
    in n
    where
        days = Data.Array.IArray.listArray (1, length expenditure) expenditure
        accum counter dayIndex =
            case lookBack d dayIndex days of
                Nothing -> counter
                Just slice -> let
                    m = median d $ mergeSort (Data.Array.IArray.elems slice)
                    today = days ! dayIndex
                    in if today >= (m * 2) then (counter + 1) else counter


lookBack :: Int -> Int -> Days -> Maybe Days
lookBack windowSize dayIndex arr
    | dayIndex - windowSize <= 0 = Nothing
    | otherwise = let
        elems = (arr ! ) <$> [(dayIndex - windowSize).. (dayIndex -1)]
        slice = Data.Array.IArray.listArray (1, windowSize) elems
        in Just slice

median :: Int -> [Int] -> Int
median len arr
    | even len = let
        (a:b:_) = Data.List.drop (len `div` 2) arr
        in ((a+b) `div` 2)
    | otherwise = arr !! (len `div` 2)

arrLen :: Days -> Int
arrLen = length

-- split in half
-- recursively sort the halves
-- combine results together
mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort [a,b] = if a > b then [b,a] else [a,b]
mergeSort days = let
    midpoint = length days `div` 2
    (lhs, rhs) = Data.List.splitAt midpoint days
    sortedLhs = mergeSort lhs
    sortedRhs = mergeSort rhs

    in combine sortedLhs sortedRhs
    where
        combine rest [] = rest
        combine [] rest = rest
        combine (l:lrest) (r:rrest)
            | l > r = r: combine (l:lrest) rrest
            | otherwise = l: combine lrest (r:rrest)

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

    ndTemp <- System.IO.getLine
    let nd = words ndTemp

    let n = read (nd !! 0) :: Int

    let d = read (nd !! 1) :: Int

    expenditureTemp <- System.IO.getLine

    let expenditure = Data.List.map (read :: String -> Int) . words $ expenditureTemp

    let result = activityNotifications expenditure d

    System.IO.hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr

