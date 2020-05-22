module DNAHealth where

import Protolude hiding (hPutStrLn, getLine)
import Prelude (String, read)

import qualified Data.List
import System.IO
import System.Environment

-- Complete the climbingLeaderboard function below.
climbingLeaderboard scores alice = let
    rankReversed = reverse $ denseRank scores
    (_, alicesRanks) = foldl' compute (rankReversed, []) alice
    in reverse alicesRanks
    where
        compute (ranks, results) aliceScore = let
            (_, rhs) = sliceScores aliceScore ranks
            in case rhs of
                [] -> (rhs, 1:results)
                ((rank, score):_) | score == aliceScore -> (rhs, rank:results)
                ((rank, _):_) -> (rhs, rank+1:results)

denseRank :: [Int] -> [(Int, Int)]
denseRank = zip [1..] . fmap Data.List.head . group

sliceScores :: Int -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
sliceScores aliceScore = Data.List.partition ( (<= aliceScore). snd)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    scoresCount <- readLn :: IO Int

    scoresTemp <- getLine

    let scores = Data.List.map (read :: String -> Int) . Data.List.words $ scoresTemp

    aliceCount <- readLn :: IO Int

    aliceTemp <- getLine

    let alice = Data.List.map (read :: String -> Int) . Data.List.words $ aliceTemp

    let result = climbingLeaderboard scores alice

    hPutStrLn fptr $ intercalate "\n" $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
