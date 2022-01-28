{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}
module BiggerGreater where

import Prelude

import Data.Foldable
import qualified Data.Sequence as S
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the biggerIsGreater function below.
biggerIsGreater w =
  nextPermutation [] wSeq
  where
    wSeq = S.fromList w

nextPermutation :: [Char] -- ^
  -> S.Seq Char -- ^
  -> [Char]
nextPermutation rhs word =
  -- Working from the rhs of the word
  case S.viewr word of
    S.EmptyR -> "no answer"
    (front S.:> c) ->
      case S.viewr front of
        S.EmptyR -> nextPermutation (c:rhs) front
        (untouched S.:> neighbor) ->
          if neighbor >= c
          then nextPermutation (c:rhs) front
          -- swapping with the first char to the left of c smaller than c
          else let
          -- reverse the rhs to guarantee that this is the next largest permutation
            (lessThan, swapElem:greaterThan) = span (<= neighbor) $ reverse (c:rhs)
            in toList untouched <> [swapElem] <> lessThan <> (neighbor:greaterThan)


main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    forM_ [1..n] $ \n_itr -> do
        w <- getLine

        let result = biggerIsGreater w

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr

