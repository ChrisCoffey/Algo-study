module AStar where

import Protolude
import qualified Data.Vector as V
import qualified Data.Text as T

newtype Board a = Board (V.Vector (Column a))
newtype Column a = Column (V.Vector a)
newtype Point = Point (Int, Int)
type Path = [Point]
type BBoard = Board Bool


findPath ::
    Point
    -> Point
    -> Board a
    -> Path
findPath start goal board =
    undefined

-- Actual algorithm would be to weight each quare by the distance to the goal for each spot
-- From there, work in both directions, choosing the square with the shortest total path
-- to the target. It should use a heap to track the current shortest path
--
-- This particular implementation uses the A* Jump Point search algorithm, which is simply a
-- version optimized for finding paths in a fixed-cost grid like a rogue map.

-- Throw a ray through an adjacent point and return the first edge it hits
castRay ::
    Point
    -> Point
    -> BBoard
    -> Point
castRay from through (Board cols) = undefined

-- Utilities and stuff

trivialBoard :: Board Bool
trivialBoard = Board $ V.replicate 60 col
    where
    col = Column $ V.replicate 60 True

prettyPrint ::
    (a -> Char)
    -> Board a
    -> Text
prettyPrint renderA (Board cols) =
    T.unlines [T.pack $ V.toList (renderA <$> unCol col) | col <- V.toList cols]
    where
    unCol (Column cx) = cx
