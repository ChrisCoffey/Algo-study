module Output (
    PrettyPrinter(..)
) where

import Protolude

class PrettyPrinter a where
    prettyPrint :: a -> Text
