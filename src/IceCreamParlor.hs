module IceCreamParlor where

import Protolude
import Prelude (read, String)
-- started: Mon Mar 30 15:40:08 EDT 2020
-- correct but inefficient: Mon Mar 30 16:08:32 EDT 2020


import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import qualified Data.IntMap as M
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the whatFlavors function below.
--whatFlavors' :: [Int] -> Int -> IO ()
--whatFlavors' costs money = do
--    let withIndexes = zip costs [1..]
--        res = runSearch (costIndexes withIndexes) withIndexes
--    System.IO.print $ (show $ fst res) <> (" "::String) <> (show $ snd res)
--    where
--        searchCosts :: M.Map Int [Int] -> (Int, Int) -> Maybe Int
--        searchCosts indexedCosts (cost, i) =
--        -- see if there is a "peer" for this value.
--        -- If there is, check that it occurs after the current index.
--        -- This is to guard against pulling the same value twice
--           find (> i) =<< M.lookup (money - cost) indexedCosts
--
--        runSearch :: M.Map Int [Int] -> [(Int, Int)] -> (Int, Int)
--        runSearch indexedCosts (x:xs) =
--            case searchCosts indexedCosts x of
--                Just secondIndex -> (snd x, secondIndex)
--                Nothing -> runSearch indexedCosts xs

whatFlavors'' :: [Int] -> Int -> IO ()
whatFlavors'' costs money = do
    let withIndexes = zip costs [1..]
        affordable = Data.List.filter ((< money) . fst) withIndexes
        res = runSearch (costIndexes withIndexes) affordable
    System.IO.print $ (show $ fst res) <> (" "::String) <> (show $ snd res)
    where
        searchCosts :: M.IntMap [Int] -> (Int, Int) -> Maybe Int
        searchCosts indexedCosts (cost, i) =
        -- see if there is a "peer" for this value.
        -- If there is, check that it occurs after the current index.
        -- This is to guard against pulling the same value twice
           find (> i) =<< M.lookup (money - cost) indexedCosts

        runSearch :: M.IntMap [Int] -> [(Int, Int)] -> (Int, Int)
        runSearch indexedCosts (x:xs) =
            case searchCosts indexedCosts x of
                Just secondIndex -> (snd x, secondIndex)
                Nothing -> runSearch indexedCosts xs

costIndexes ::
    [(Int, Int)]
    -> M.IntMap [Int]
costIndexes indexed =
    Data.List.foldl' f M.empty indexed
    where
        f acc x@(cost, i) = M.insertWith (<>) cost [i] acc

whatFlavors :: [Int] -> Int -> M.IntMap Int -> Int -> IO ()
whatFlavors [] _ _ _ = System.IO.putStrLn ("boom"::String)
whatFlavors (cost:rest) money acc n =
    case M.lookup cost acc of
        Just i -> System.IO.putStrLn $ show i <> (" "::String) <> show n
        Nothing -> let
            acc' = M.insert (money - cost) n acc
            in whatFlavors rest money acc' (n+1)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- System.IO.getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        money <- readLn :: IO Int

        n <- readLn :: IO Int

        costTemp <- System.IO.getLine

        let cost = Data.List.map (read :: String -> Int) . words $ costTemp

        whatFlavors cost money M.empty 1
