module ArrayChaos where

-- start: Sun Mar  8 11:35:18 EDT 2020
-- working: Sun Mar  8 12:41:40 EDT 2020

import Prelude (String, read, readLn)
import Protolude

import Data.List (words)
import qualified Data.Text as T
import qualified Data.Set as S

-- Complete the minimumBribes function below.
minimumBribes :: [Int] -> IO ()
minimumBribes q
    | invalidInput indexed = putStrLn "Too chaotic"
    | otherwise = print $ lessThanBehind q
    where
        indexed = zip q [1..]


-- I only figured this relationship out in the last 6 minutes
lessThanBehind :: [Int] -> Int
lessThanBehind [] = 0
lessThanBehind (x:rest) =
    numLessThan 0 rest + lessThanBehind rest
    where
        numLessThan :: Int -> Int -> [Int] -> Int
        numLessThan 2 _ _ =  2
        numLessThan n 100 _ = n
        numLessThan n counter (a:as)
            | a < x = numLessThan (n+1) (counter+1) as
            | otherwise = numLessThan n (counter+1) as
        numLessThan n _ [] = n

invalidInput :: [(Int, Int)] -> Bool
invalidInput =
    any ( (> 2) . distance)
    where
        distance (sticker, spot) = sticker - spot

main :: IO()
main = do
    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        n <- readLn :: IO Int

        qTemp <- getLine

        let q = map (read . T.unpack :: Text -> Int) . T.words $ qTemp

        minimumBribes q
