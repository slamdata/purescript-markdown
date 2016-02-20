module Text.Markdown.SlamDown.Syntax.Value
  ( Value
  , stringValue
  , renderValue
  ) where

import Prelude

class (Eq a) <= Value a where
  stringValue :: String -> a

  renderValue :: a -> String

instance valueString :: Value String where
  stringValue = id
  renderValue = id
