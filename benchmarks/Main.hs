import Criterion.Main
import Protolude

import qualified Benchmarks.NQueens as NQ

main :: IO ()
main = defaultMain [
    NQ.benchmarks
    ]
