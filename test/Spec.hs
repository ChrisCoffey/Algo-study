import qualified IndexedTests as IT
import qualified NQueensTests as NQ

import Protolude
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Kitchen Sink" [
    IT.tests,
    NQ.tests
    ]


