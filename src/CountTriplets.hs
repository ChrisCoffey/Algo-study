module CountTriplets where

-- start Sun Feb 23 14:05:17 EST 2020
-- non-ideal working Sun Feb 23 14:36:17 EST 2020
-- ideal working

import Protolude hiding (State, evalState)
import Prelude(String, read)

import Control.Monad
import Control.Monad.State.Strict
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import qualified Data.Map as M
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

type Memo = M.Map Integer Integer

-- Complete the countTriplets function below.
countTriplets ::
    [Integer]
    -> Integer
    -> Integer
countTriplets arr r =
    M.foldlWithKey' accumulate 0 arrLookup
    where
        arrLookup = arrValueIndexes arr

        accumulate ::
            Integer
            -> Integer
            -> [Integer]
            -> Integer
        accumulate acc x indexes =
            case (M.lookup (x*r) arrLookup, M.lookup (x*r*r) arrLookup) of
                (Nothing, _) -> acc
                (_, Nothing) -> acc
                (Just bs, Just cs) -> acc + distinctTriples M.empty indexes bs cs

-- Add a chck for runs of a to reduce duplicate nmValid calls
distinctTriples _ [] _ _ = 0
distinctTriples _ _ [] _ = 0
distinctTriples _ _ _ [] = 0
distinctTriples memo (a:as) (b:bs) (c:cs)
    | a < b && b < c =
        case M.lookup b memo of
            Just n -> let
                d = distinctTriples memo as (b:bs) (c:cs)
                in n+d
            Nothing -> let
                (n, memo') = numValid memo (b:bs) (c:cs)
                d = distinctTriples memo' as (b:bs) (c:cs)
                in n + d
    | a >= b = distinctTriples memo (a:as) (Data.List.dropWhile (< a) bs) (c:cs)
    | otherwise = let
        (dropped, cs') = Data.List.span (< b) cs
        in distinctTriples memo (a:as) (b:bs) cs'

        -- takes two lists in descending order
        -- Determines the length of
numValid ::
    Memo
    -> [Integer]
    -> [Integer]
    -> (Integer, Memo)
numValid m [] _ = (0, m)
numValid m _ [] = (0, m)
numValid memo (x:xs) (y:ys)
    | x < y = let
        (n, memo') = numValid memo xs (y:ys)
        res = n + 1
        memo'' = M.insert x res memo
        in (res, memo'')
    | otherwise =
        numValid memo (x:xs) ys


-- Returns a Map with values and a sorted list of the indexes it occurs at
arrValueIndexes :: [Integer] -> M.Map Integer [Integer]
arrValueIndexes arr = let
    withIndexes = Data.List.zip arr [0..]
    raw = Data.List.foldl' accumulate M.empty withIndexes
    in Data.List.sort <$> raw
    where
        accumulate acc (x,idx) = M.insertWith (<>) x [idx] acc


-----------------------------------
--
--      Single-loop impl
--
-----------------------------------
countTriplets' ::
    [Integer]
    -> Integer
    -> Integer
countTriplets' ls r = let
    (_ , _, count) = Data.List.foldl' accumulate (M.empty, M.empty, 0) ls
    in count
    where
        accumulate :: (Memo, Memo, Integer) -> Integer -> (Memo, Memo, Integer)
        accumulate (singles, pairs, count) n
            | n `mod` r /= 0 = (singles', pairs, count)
            | otherwise = let
                pairs' = M.insertWith (+) n (M.findWithDefault 0 key singles) pairs
                count' = count + M.findWithDefault 0 key pairs
                in (singles', pairs', count')
            where
                key = n `div` r
                singles' = M.insertWith (+) n 1 singles



lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

run :: IO()
run = do
    rows <- Data.Text.lines <$> Protolude.readFile "data/count_triplets.dat"
    let nr = Data.Text.words . Data.Text.stripEnd $ Data.List.head  rows

    let n = read (show $ Data.List.head nr ) :: Int
        r = read (Data.Text.unpack (Data.List.last nr)) :: Integer
        arrTemp = Data.List.last rows

        arr = Data.List.map (read . Data.Text.unpack) . Data.Text.words $ Data.Text.stripEnd arrTemp

        ans = countTriplets' arr r

    Protolude.print ans
