module SpecialString where

import Protolude
import Prelude(String)

--  start Mon Feb 17 13:51:19 EST 2020
--
--  end: basic Mon Feb 17 14:36:01 EST 2020
--
--  end: optimized 16:20:01 EST 2020

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe


-- The problem asks us to solve for finding "special strings" within a larger string.
-- A special string is a palindrome comprised of at most two distinct letters in one of two patters.
-- 1) all letters are the same
-- 2) Only two letters are used, with only the middle being different
--
-- Some examples, `a`, `aa`, `aaaa`, `aba`, `aaaaaaabaaaaaaa`
-- Not special strings: `ab`, `abba`, `ababa`
--
-- From this, we're asked to find a function for identifying how many speical strings are present in a given string.
--
-- I initially broke this down into a few sub problems:
-- 1) determine if a string is special
-- 2) check if the tail of the string is special
--
-- This lets us write an algorithm that starts at the left hand side of the string and walks towards the end.
-- It is also very wrong, since of the following possible special strings, it only finds half of them:
-- `aaaa` -> `aaaa`,`aaa`, `aaa`, `aa`, `aa`, `aa`, `a`,`a`,`a`,`a`
--
-- As you can see, it completely ignores anythign embedded in the middle, and it also doesn't correctly pick up anything shorter than `s` that doesn't end at the end of the string.
--
-- So, this is a pretty wrong algorithm, but it was useful for identifying a correct pattern for solving the algorithm.
substrCount_Wrong :: Int -> String -> IO Int
substrCount_Wrong _ [a] = pure 1
substrCount_Wrong _ [a,b] = do
    let x = if a == b then 1 else 0
    a' <- substrCount_Wrong 0 [a]
    b' <- substrCount_Wrong 0 [ b ]
    pure $ a' + b' + x
substrCount_Wrong _ s
    | even (length s) && allSame = do
        x <- substrCount_Wrong 0 remainder
        pure $ 1 + x
    | odd (length s) = do
        let len = length s `div` 2
            start = Data.List.take len s
            end = Data.List.drop (len + 1) s
            matches = start == end && checkAllSame start
        left <- substrCount_Wrong 0 start
        right <- substrCount_Wrong 0 end
        pure $ left + right + 1 + (if matches then 1 else 0)
    | otherwise = substrCount_Wrong 0 remainder
    where
        allSame = checkAllSame remainder
        checkAllSame = all (\c -> c == Data.List.head s)
        remainder = tail s

-- Works but inefficient

-- Generate all subsequences of length N from the source string
slidingWindow ::
    String
    -> Int
    -> [String]
slidingWindow str n
    | length str == n = [str]
    | otherwise = let
        xs = Data.List.take n str
        in xs : slidingWindow (tail str) n

substrCount :: String -> Int
substrCount [] = 0
substrCount str =
    length $ Data.List.filter isSpecial subs
    where
        subs = subseqs str

        isSpecial [c] = True
        isSpecial s = allMatch s || headTailMatch s

        allMatch s = all (\c -> c == Data.List.head s) s
        headTailMatch s = let
            h = Data.List.take (Data.List.length s `div` 2) s
            t = Data.List.drop ((Data.List.length s `div` 2) + 1) s
            in h == t

subseqs :: String -> [String]
subseqs str =
    concatMap (slidingWindow str) [1..length str]

-- Works and efficient (?)

subStrCountFast ::
    String
    -> Int
subStrCountFast str =
    palindromeCount + runScores
    where
        palindromeCount = countPalindromes runs

        runs = encodeRuns str

        runScores = Data.List.sum $ runSubStrs <$> runs


countPalindromes ::
    [(Char, Int)]
    -> Int
countPalindromes [(a, an), (b,1), (c,cn)]
    | a == c = min an cn
    | otherwise = 0
countPalindromes [a,b,c] = 0
countPalindromes ((a, an):(b,1):(c,cn):rest)
    | a == c = let
        palindromeCount = min an cn
        in palindromeCount + countPalindromes ((b,1):(c,cn):rest)
    | otherwise = countPalindromes ((b,1):(c,cn):rest)
countPalindromes (a:b:c:rest) = countPalindromes (b:c:rest)
countPalindromes _ = 0

runSubStrs ::
    (Char, Int)
    -> Int
runSubStrs (_, x) = (x * (x+1)) `div` 2

encodeRuns ::
    String
    -> [(Char, Int)]
encodeRuns [] = []
encodeRuns (x:rest) = let
    len = run rest
    in (x, len + 1) : encodeRuns (Data.List.drop len rest)
    where
        run [] = 0
        run (a:as)
            | a == x = 1 + run as
            | otherwise = 0


main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    n <- readLn :: IO Int

    s <- System.IO.getLine

    let result = subStrCountFast s

    System.IO.hPrint fptr result

    hFlush fptr
    hClose fptr

