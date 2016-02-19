module Text.Markdown.SlamDown.Syntax
  ( SlamDown(..)

  , module Text.Markdown.SlamDown.Syntax.FormField
  , module Text.Markdown.SlamDown.Syntax.Inline
  , module Text.Markdown.SlamDown.Syntax.Block
  ) where

import Prelude

import Data.Function (on)
import Data.List as L
import Data.Monoid (Monoid, mempty)
import Test.StrongCheck
import Test.StrongCheck.Gen

import Text.Markdown.SlamDown.Syntax.FormField
import Text.Markdown.SlamDown.Syntax.Inline
import Text.Markdown.SlamDown.Syntax.Block

data SlamDown = SlamDown (L.List Block)

instance showSlamDown :: Show SlamDown where
  show (SlamDown bs) = "(SlamDown " ++ show bs ++ ")"

instance eqSlamDown :: Eq SlamDown where
  eq (SlamDown bs1) (SlamDown bs2) = bs1 == bs2

instance ordSlamDown :: Ord SlamDown where
  compare = compare `on` show

instance semigroupSlamDown :: Semigroup SlamDown where
  append (SlamDown bs1) (SlamDown bs2) = SlamDown (bs1 <> bs2)

instance monoidSlamDown :: Monoid SlamDown where
  mempty = SlamDown mempty

instance arbitrarySlamDown :: Arbitrary SlamDown where
  arbitrary = SlamDown <<< L.toList <$> arrayOf arbitrary

