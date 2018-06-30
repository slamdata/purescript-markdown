module Text.Markdown.SlamDown.Syntax.Value
  ( class Value
  , stringValue
  , renderValue
  ) where

import Prelude

class (Eq a, Ord a) ⇐ Value a where
  stringValue
    ∷ String
    → a
  renderValue
    ∷ a
    → String

instance valueString ∷ Value String where
  stringValue = identity
  renderValue = identity
