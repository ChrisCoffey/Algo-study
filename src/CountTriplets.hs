module CountTriplets where

-- start Sun Feb 23 14:05:17 EST 2020
-- non-ideal working Sun Feb 23 14:36:17 EST 2020
-- ideal working

import Protolude
import Prelude(String, read)

import Control.Monad
import Control.Monad.State
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
    M.foldlWithKey accumulate 0 arrLookup
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
                (Just bs, Just cs) -> acc + distinctTriples indexes bs cs

        distinctTriples [] _ _ = 0
        distinctTriples _ [] _ = 0
        distinctTriples _ _ [] = 0
        distinctTriples (a:as) (b:bs) (c:cs)
            | a < b && b < c = let
                n = numValid (b:bs) (c:cs)
                d = distinctTriples as (b:bs) (c:cs)
                in n + d
            | a >= b = distinctTriples (a:as) bs (c:cs)
            | otherwise = distinctTriples (a:as) (b:bs) cs

        numValid ::
            [Integer]
            -> [Integer]
            -> Integer
        numValid [] _ = 0
        numValid _ [] = 0
        numValid (x:xs) (y:ys)
            | x < y = let
                (lesser,greater) = Data.List.span (< y) (x:xs)
                runTotal = Data.List.length lesser *  Data.List.length (y:ys)
                in fromIntegral runTotal + numValid greater (y:ys)
            | otherwise = numValid (x:xs) $ Data.List.dropWhile (< x) ys


-- Returns a Map with values and a sorted list of the indexes it occurs at
arrValueIndexes :: [Integer] -> M.Map Integer [Integer]
arrValueIndexes arr = let
    withIndexes = Data.List.zip arr [0..]
    raw = Data.List.foldl' accumulate M.empty withIndexes
    in Data.List.sort <$> raw
    where
        accumulate acc (x,idx) = M.insertWith (<>) x [idx] acc

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    rows <- Data.Text.lines <$> Protolude.readFile "data/count_triplets.dat"
    let nr = Data.Text.words . Data.Text.stripEnd $ Data.List.head  rows

    let n = read (show $ Data.List.head nr ) :: Int
        r = read (Data.Text.unpack (Data.List.last nr)) :: Integer
        arrTemp = Data.List.last rows

        arr = Data.List.map (read . Data.Text.unpack) . Data.Text.words $ Data.Text.stripEnd arrTemp

        ans = countTriplets arr r

    Protolude.print ans
