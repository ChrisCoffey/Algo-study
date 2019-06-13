module Benchmarks.NQueens (
    benchmarks
) where

import NQueens

import Protolude
import Criterion.Main

benchmarks :: Benchmark
benchmarks = bgroup "NQueens" [
     bench "naive 5x5" $ whnf nQueens1 5
    ,bench "naive 8x8" $ whnf nQueens1 8
    ,bench "naive 10x10" $ whnf nQueens1 10
    ]
