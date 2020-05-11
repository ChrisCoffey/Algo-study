module OrganizingContainers where

import Protolude

import qualified Data.Array as A
import qualified Data.Map as M

organizingContainers :: [[Int]] -> Bool
organizingContainers container = let
    sums = zipWith (-) incorrectCounts freeCounts
    in all (== 0) sums
    where
        arrContainers = A.listArray (0, n-1) $ A.listArray (0, n-1) <$> container
        n = length container
        incorrectCounts = incorrectPerBucket <$> [0..n-1]
        freeCounts = freeBalls <$> [0..n-1]

        -- all values in the bucket that are not of the same type as the bucket
        incorrectPerBucket idx = let
            bucket = arrContainers A.! idx
            range = rangeExceptIndex idx
            in sum $ (bucket A.!) <$> range

        -- all values of type t in the matrix, that don't fall on row n
        freeBalls idx = let
            range = rangeExceptIndex idx
            vals = (\i -> (A.! idx) $ arrContainers A.! i)<$> range
            in sum vals

        rangeExceptIndex idx = [x | x <- [0..n-1], x /= idx]
