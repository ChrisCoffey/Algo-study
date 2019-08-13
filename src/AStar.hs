module AStar where

import Protolude
import qualified Data.Vector as V
import qualified Data.Text as T

newtype Board a = Board (V.Vector (Column a))
newtype Column a = Column (V.Vector a)
newtype Point = Point (Int, Int)
type Path = [Point]

findPath ::
    Point
    -> Point
    -> Board a
    -> Path
findPath start goal board =
    undefined

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
