module Indexed (
    Indexed(..)
    ) where

import Protolude
import Numeric.Natural

class Indexed f i where
    get :: f a -> i -> Maybe a
    set :: f a -> i -> a -> Maybe (f a)

instance Indexed [] Natural where
    get xs i
        | fromIntegral i >= length xs = Nothing
        | otherwise = headMay $ drop (fromIntegral i) xs

    set xs i newValue
        | fromIntegral i >= length xs = Nothing
        | otherwise = let
            front = take (fromIntegral i) xs
            back = tailMay $ drop (fromIntegral i) xs
            in (front <>) . ( newValue:) <$> back
