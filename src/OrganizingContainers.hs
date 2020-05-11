module OrganizingContainers where

import Protolude

import qualified Data.Array as A
import qualified Data.Map as M

organizingContainers :: [[Int]] -> Bool
organizingContainers container = let
    sums = zipWith (-) (sort incorrectCounts) (sort freeCounts)
    in all (== 0) sums
    where
        arrContainers = A.listArray (0, n-1) $ A.listArray (0, n-1) <$> container
        n = length container
        range = [0..n-1]
        incorrectCounts = incorrectPerBucket <$> range
        freeCounts = freeBalls <$> range

        -- all values in the bucket that are not of the same type as the bucket
        incorrectPerBucket idx = let
            bucket = arrContainers A.! idx
            in sum $ (bucket A.!) <$> range

        -- all values of type t in the matrix, that don't fall on row n
        freeBalls idx = let
            vals = (\i -> (A.! idx) $ arrContainers A.! i) <$> range
            in sum vals
