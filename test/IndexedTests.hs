module IndexedTests (
    tests
) where

import Indexed

import Numeric.Natural
import Protolude hiding (get, set)
import Test.QuickCheck.Instances.Natural ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Indexed" [
    indexedLists
    ]

indexedLists :: TestTree
indexedLists = testGroup "Lists" [
    testProperty "get considers boundaries" $ \(ls :: [Int], idx) ->
        cover 20 (idx > lengthN ls) "too large" $
        cover 20 (idx < lengthN ls) "less than" $
        cover 5 (idx == lengthN ls) "boundary" $
        let v = ls `get` idx
        in if idx < lengthN ls
           then isJust v
           else isNothing v
    ,
    testProperty "get returns the value for valid indices" $ \(ls :: [Int], idx) ->
        cover 20 (idx > lengthN ls) "too large" $
        cover 20 (idx < lengthN ls) "less than" $
        cover 5 (idx == lengthN ls) "boundary" $
        let v = ls `get` idx
        in if idx < lengthN ls
           then v == headMay (drop (fromIntegral idx) ls)
           else isNothing v
    ,
    testProperty "set considers boundaries" $ \(ls :: [Int], idx, a) ->
        cover 20 (idx > lengthN ls) "too large" $
        cover 20 (idx < lengthN ls) "less than" $
        cover 5 (idx == lengthN ls) "boundary" $
        let v = set ls idx a
        in if idx < lengthN ls
           then isJust v
           else isNothing v
    ,
    testProperty "set changes the value for valid indices" $ \(ls :: [Int], idx, a) ->
        cover 20 (idx > lengthN ls) "too large" $
        cover 20 (idx < lengthN ls) "less than" $
        cover 5 (idx == lengthN ls) "boundary" $
        let v = (`get` idx) =<< set ls idx a
        in if idx < lengthN ls
           then v == Just a
           else isNothing v
    ]

lengthN ::
    [a]
    -> Natural
lengthN = fromIntegral . length

