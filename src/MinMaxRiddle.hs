module MinMaxRiddle where

-- start: Sun Apr 26 14:58:43 EDT 2020

import Protolude

import Prelude(head, read, String)

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.Map as M
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

type Arr = Array Int Integer
newtype LHS = LHS Int
newtype RHS = RHS Int
newtype MaxMin = MM Integer

riddle' :: [Integer]-> [Integer]
riddle' rawInput = let
    sizes = go arr 0 M.empty
    mms = Data.List.foldl (f sizes) [] [upperBound + 1, upperBound..1]
    in mms
    where
        upperBound = length rawInput - 1
        arr = listArray (0, upperBound) rawInput

        go arr index sizes
            | index > snd (bounds arr) = transposeM sizes
            | otherwise = let
                sz = windowSize arr index
                value = arr ! index
                lookup' = M.insertWith max value sz sizes
                in go arr (index + 1) lookup'

        -- The idea is that if an element has a maximum window size of x, then it can satisfy any size
        -- less than x as well.
        f sizes acc w =
            case M.lookup w sizes of
                Nothing -> h : acc
                Just v -> (max v h):acc
            where
                h = Data.List.head acc

transposeM :: (Ord a, Ord b, Show a, Show b) => Map a b -> Map b a
transposeM = M.foldlWithKey (\ acc k v -> M.insertWith max v k acc) M.empty

-- Computing window sizes by walking from each element left + right until values smaller are found
windowSize ::
    Arr
    -> Int
    -> Int
windowSize arr index
    | index == fst (bounds arr) = 1 + distance (+) index (arr ! index)
    | index == snd (bounds arr) = 1 + distance (-) index (arr ! index)
    | otherwise = 1 + distance (+) index (arr ! index) + distance (-) index (arr ! index)
    where
        distance op i value = let
            i' = i `op` 1
            in if outOfBounds i' || value > arr ! i'
               then 0
               else 1 + distance op i' value

        outOfBounds i = i > snd (bounds arr) || i < fst (bounds arr)


-- Point out the fatal flaw in this code. I didn't test this on enough inputs

-- Complete the riddle function below.
riddle :: [Integer] -> [Integer]
riddle rawInput = let
    (result, _, _, _) = Data.List.foldl' (\(seen, m, l, r) _ -> step arr seen m l r)
                                         ([arr ! maxIndex], MM (arr ! maxIndex), LHS $ maxIndex - 1, RHS $ maxIndex + 1)
                                         (tail rawInput)
    in reverse result
    where
        upperBound = length rawInput - 1
        arr = listArray (0, upperBound) rawInput

        -- find the index of the largest value
        maxIndex = Data.List.foldl' (\m i -> if (arr ! i) > (arr ! m) then i else m) 0 [0..upperBound]

-- When in doubt, steps left
step ::
    Arr
    -> [Integer] -- seen values
    -> MaxMin
    -> LHS
    -> RHS
    -> ([Integer], MaxMin, LHS, RHS)
step arr seen (MM m) (LHS l) (RHS r)
    -- when at a boundary, can only compare one side to the min value
    | l < fst (bounds arr) = let
        x = arr ! r
        newMin = if x < m then x else m
        in (newMin:seen, MM newMin, LHS l, RHS $ r+1)
    | r > snd (bounds arr) = let
        x = arr ! l
        newMin = if x < m then x else m
        in (newMin: seen, MM newMin, LHS $ l-1, RHS r)

    -- compare the larger of the two to the minimum value
    | arr ! l > arr ! r = let
        x = arr ! l
        newMin = if x < m then x else m
        in (newMin:seen, MM newMin, LHS (l-1), RHS r)
    | otherwise = let
        x = arr ! r
        newMin = if x < m then x else m
        in (newMin:seen, MM newMin, LHS l, RHS $ r+1)

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

    arrTemp <- System.IO.getLine

    let arr = Data.List.map (read :: String -> Integer) . words $ arrTemp

    let res = riddle arr

    System.IO.hPutStrLn fptr $ intercalate " " $ Data.List.map (\x -> show x) $ res

    hFlush fptr
    hClose fptr
