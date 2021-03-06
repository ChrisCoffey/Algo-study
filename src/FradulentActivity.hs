module FradulentActivity where

-- start: Thu Apr  9 08:25:29 EDT 2020
-- end: Not working
-- correct but slow at Thu Apr  9 09:40:09 EDT 2020

import Protolude
import Prelude(String, read)

import Control.Monad
import Data.Array.IArray
import Data.Bits
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.IntMap as M
import qualified Data.HashTable.IO as H
import Data.Set as S
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--data FastWindow = W {front :: [Int], back :: [Int], counts :: HashTable, wsize :: Int}
--type HashTable =  H.BasicHashTable Int Int
--
--fastSolve :: [Int] -> Int -> IO Int
--fastSolve arr d = do
--    ht <- H.new :: IO HashTable
--    cxs ht ls
--    go (W {front= ls, back = [], counts = ht, wsize = d}) rest
--    where
--        (ls, rest) = Data.List.splitAt d arr
--
--        cxs :: HashTable -> [Int] -> IO ()
--        cxs ht [] = pure ()
--        cxs ht (n:rest) = do
--            H.mutate ht n updateFunc
--            cxs ht rest
--
--        updateFunc Nothing =  (Just 1, ())
--        updateFunc (Just x) = (Just (x+1), ())
--
--        go :: FastWindow -> [Int] -> IO Int
--        go _ [] = pure 0
--        go w (x:xs) = do
--            med <- medianFW w
--            w' <- advanceWindow w x
--            if med * 2 <= fromIntegral x
--            then (1 +) <$> go w' xs
--            else go w' xs
--
--advanceWindow ::
--    FastWindow
--    -> Int
--    -> IO FastWindow
--advanceWindow fw a = do
--    let (x:rest) = front rotatedFW
--    H.mutate (counts fw) a incOrCreate
--    H.mutate (counts fw) x decOrDelete
--    pure $ fw {front = rest, back = a:back fw}
--    where
--        rotatedFW = case front fw of
--            [] -> fw {front = reverse (back fw), back = []}
--            _ -> fw
--
--        incOrCreate Nothing = (Just 1, ())
--        incOrCreate (Just v) = (Just (v+1), ())
--
--        decOrDelete Nothing = (Nothing, ())
--        decOrDelete (Just 1) = (Nothing, ())
--        decOrDelete (Just v) = (Just (v-1), ())
--
--medianFW :: FastWindow -> IO Double
--medianFW fw =
--    go ((wsize fw `div` 2)+1) [1..200]
--    where
--        ht = counts fw
--
--        go _ [] = pure 0
--        go sz (n:rest) = do
--            mv <- H.lookup ht n
--            case mv of
--                Nothing -> go sz rest
--                Just c -> do
--                    let sz' = sz - c
--                    case sz' <= 0 of
--                        True | even (wsize fw) && sz >= 2 ->
--                            pure $ fromIntegral n
--                        True | even (wsize fw) -> do
--                            (n':_) <- catMaybes <$> sequence [H.lookup ht a | a <- [n, n-1..1]]
--                            pure $ fromIntegral (n + n') / 2
--                        True -> pure $ fromIntegral n
--                        _ -> go sz' rest
--

data FastWindow = W {front :: [Int], back :: [Int], counts :: M.IntMap Int, wsize :: Int}

fastSolve :: [Int] -> Int -> Int
fastSolve arr d =
    go initialWindow rest
    where
        (ls, rest) = Data.List.splitAt d arr
        cxs = Data.List.foldl (\acc n -> M.insertWith (+) n 1 acc) M.empty ls
        initialWindow = W {
            front = ls,
            back = [],
            counts = cxs,
            wsize = d
            }

        go _ [] = 0
        go w (x:xs) = let
            med = medianFW w
            w' = advanceWindow w x
            in if med * 2 <= fromIntegral x
               then 1 + go w' xs
               else go w' xs

advanceWindow ::
    FastWindow
    -> Int
    -> FastWindow
advanceWindow fw a = let
    (x:rest) = front rotatedFW
    counts' = M.insertWith (+) a 1 $ counts fw
    counts'' = case M.lookup x counts' of
        Just 1 -> M.delete x counts'
        Just x | x <= 0 -> M.delete x counts'
        _ -> M.insertWith (flip (-)) x 1 counts'
    in fw {front = rest, back = a:back fw, counts = counts''}
    where
        rotatedFW = case front fw of
            [] -> fw {front = reverse (back fw), back = []}
            _ -> fw

medianFW :: FastWindow -> Double
medianFW fw =
    go ((wsize fw `div` 2)+1) [1..200]
    where
        cxs = counts fw

        go _ [] = 0
        go sz (n:rest) = let
            c = M.findWithDefault 0 n cxs
            sz' = sz - c
            in case sz' <= 0 of
                True | even (wsize fw) && sz >= 2 -> fromIntegral n
                True | even (wsize fw) -> let
                    n' = maximum [ a | a <- [n, n-1..1], M.member a cxs ]
                    in fromIntegral (n + n') / 2
                True -> fromIntegral n
                _ -> go sz' rest

--data Window =
--    Window
--        { small :: S.Set Int
--        , large :: S.Set Int
--        , queue :: [Int]
--        , queue' :: [Int]
--        }
--
--lookback'' :: Window -> [Int] -> (Window, [Int], Double)
--lookback'' w (x:rest) =
--    (dropOldValue w, rest, med)
--    where
--        med = medianW w
--
--        -- Drops the old value from the window, and inserts the current.
--        -- produces an updated window with balanced heaps
--        dropOldValue Window{small, large, queue, queue'}= let
--            l = S.findMin large
--            s = S.findMax small
--            oldElem = Data.List.head queue
--            in case (x > l, oldElem > l) of
--                -- Simple cases with no rotation
--               (True, True) -> w {queue' = x:queue', large = S.insert x $ S.delete oldElem large}
--               (False, False) -> w {queue' = x:queue', small = S.insert x $ S.delete oldElem small}
--
--                -- keep them balanced. a bit more complicated
--                --
--                -- Add to the larger half, remove from the smaller. Shifts the larger half's size
--                -- up by 2 relative to the smaller's prior size.
--               (True, False) | S.size small > S.size large ->
--                    w {queue' = x:queue', small = S.delete oldElem small, large = S.insert x large}
--               (True, False) -> let
--                    small' = S.insert l $ S.delete oldElem small
--                    large' = S.delete l $ S.insert x large
--                    in w {queue' = x:queue', small = small', large = large'}
--
--                --  Add to the smaller half, remove from the larger. Shifts the smaller half's size
--               (False, True) | S.size small < S.size large ->
--                    w {queue' = x:queue', small = S.insert x small, large = S.delete oldElem large}
--               (False, True) -> let
--                    small' = S.delete s $ S.insert x small
--                    large' = S.insert s $ S.delete oldElem large
--                    in w {queue' = x:queue', small = small', large = large'}
--
--
--prepareLookback :: Window -> Window
--prepareLookback w
--    | Data.List.null (queue w) = w { queue = reverse (queue' w), queue' = [] }
--    | otherwise = w
--
--medianW :: Window -> Double
--medianW Window {small, large}
--    | S.size small > S.size large = fromIntegral $ S.findMax small
--    | S.size small < S.size large = fromIntegral $ S.findMin large
--    | otherwise = fromIntegral (S.findMax small +  S.findMin large) / 2
--
--solve :: [Int] -> Int -> Int
--solve arr d =
--    go w rest
--    where
--        go _ [] = 0
--        go w xs@(x:_) = let
--            (w', next, med) = lookback'' w xs
--            in if ((Debug.Trace.trace (show med) med ) * 2) > fromIntegral x
--               then 1 + go w' next
--               else go w' next
--
--        -- setup
--        (initial, rest) = Data.List.splitAt d arr
--        (lhs, rhs) = Data.List.splitAt (d `div` 2) $ Data.List.sort initial
--        w = Window {
--            queue = initial,
--            queue' = [],
--            small = S.fromList lhs,
--            large = S.fromList rhs
--            }
--
--
--type Days = Array Int Int
--
--
----activityNotifications' :: [Int] -> Int -> Int
----activityNotifications' expenditures d =
----    case lookBack' d expenditures of
----        Nothing -> 0
----        Just (today, window, rest) -> let
----           med = median window
----           x = if today >= (2*med) then 1 else 0
----           in x + activityNotifications' rest d
----
----lookBack' :: Int -> [Int] -> Maybe (Int, [Int], [Int])
----lookBack' len expenditure@(_:rest) =
----    case accum expenditure len [] of
----        Nothing -> Nothing
----        Just (window, day) -> Just (day, window, rest)
----    where
----        accum [] n res = Nothing
----        accum (x:xs) 0 res = Just (res, x)
----        accum (x:xs) n res = accum xs (n-1) (insSort x res)
----
----        insSort n [] = [n]
----        insSort n (x:rest)
----            | n > x = x : insSort n rest
----            | otherwise = n:x:rest
----
------ Complete the activityNotifications function below.
----activityNotifications :: [Int] -> Int -> Int
----activityNotifications expenditure d = let
----    ixs = indices days
----    n = Data.List.foldl' accum 0 ixs
----    in n
----    where
----        days = Data.Array.IArray.listArray (1, length expenditure) expenditure
----        accum counter dayIndex =
----            case lookBack d dayIndex days of
----                Nothing -> counter
----                Just slice -> let
----                    m = median d $ mergeSort (Data.Array.IArray.elems slice)
----                    today = days ! dayIndex
----                    in if today >= (m * 2) then (counter + 1) else counter
----
----
----lookBack :: Int -> Int -> Days -> Maybe Days
----lookBack windowSize dayIndex arr
----    | dayIndex - windowSize <= 0 = Nothing
----    | otherwise = let
----        elems = (arr ! ) <$> [(dayIndex - windowSize).. (dayIndex -1)]
----        slice = Data.Array.IArray.listArray (1, windowSize) elems
----        in Just slice
----
----median :: Int -> [Int] -> Int
----median len arr
----    | even len = let
----        (a:b:_) = Data.List.drop (len `div` 2) arr
----        in ((a+b) `div` 2)
----    | otherwise = arr !! (len `div` 2)
----
----arrLen :: Days -> Int
----arrLen = length
----
------ split in half
------ recursively sort the halves
------ combine results together
----mergeSort :: [Int] -> [Int]
----mergeSort [x] = [x]
----mergeSort [a,b] = if a > b then [b,a] else [a,b]
----mergeSort days = let
----    midpoint = length days `div` 2
----    (lhs, rhs) = Data.List.splitAt midpoint days
----    sortedLhs = mergeSort lhs
----    sortedRhs = mergeSort rhs
----
----    in combine sortedLhs sortedRhs
----    where
----        combine rest [] = rest
----        combine [] rest = rest
----        combine (l:lrest) (r:rrest)
----            | l > r = r: combine (l:lrest) rrest
----            | otherwise = l: combine lrest (r:rrest)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- System.IO.getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do

    expenditureTemp <- (fmap (\r -> let Just (int, _) = B.readInt r in int) <$> B.words) <$> B.readFile "data/big_lookback_input.txt"

    let result = fastSolve expenditureTemp 30000
    System.IO.print result


