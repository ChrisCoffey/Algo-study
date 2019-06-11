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
        | i >= length xs = Nothing
        | otherwise = Just . head $ drop i xs

    set xs i newValue
        | i >= length xs = Nothing
        | otherwise = let
            front = take i
            back = tail $ drop i
            in front<>(newValue:back)
