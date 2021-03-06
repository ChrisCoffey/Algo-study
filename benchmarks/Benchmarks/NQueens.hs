module Benchmarks.NQueens (
    benchmarks
) where

import NQueens

import Protolude
import Criterion.Main

benchmarks :: Benchmark
benchmarks = bgroup "NQueens" [
     bench "naive 5x5" $ whnf (length . nQueens1) 5
    ,bench "naive 8x8" $ whnf (length . nQueens1) 8
    ,bench "naive 10x10" $ whnf (length . nQueens1) 10

    ,bench "matrix 5x5" $ whnf (length . nQueens2) 5
    ,bench "matrix 8x8" $ whnf (length . nQueens2) 8
    ,bench "matrix 10x10" $ whnf (length . nQueens2) 10
    ]
