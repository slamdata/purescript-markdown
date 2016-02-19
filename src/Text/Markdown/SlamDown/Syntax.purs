module Text.Markdown.SlamDown.Syntax
  ( SlamDownP(..)
  , SlamDown()

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

-- | `SlamDownP` is the type of SlamDown abstract syntax trees which take values in `a`.
data SlamDownP a = SlamDown (L.List (Block a))

type SlamDown = SlamDownP String

instance functorSlamDownP :: Functor SlamDownP where
  map f (SlamDown bs) = SlamDown (map f <$> bs)

instance showSlamDownP :: (Show a) => Show (SlamDownP a) where
  show (SlamDown bs) = "(SlamDown " ++ show bs ++ ")"

instance eqSlamDownP :: (Eq a) => Eq (SlamDownP a) where
  eq (SlamDown bs1) (SlamDown bs2) = bs1 == bs2

-- TODO: replace this with a proper `Ord` instance.
instance ordSlamDownP :: (Show a, Eq a) => Ord (SlamDownP a) where
  compare = compare `on` show

instance semigroupSlamDownP :: Semigroup (SlamDownP a) where
  append (SlamDown bs1) (SlamDown bs2) = SlamDown (bs1 <> bs2)

instance monoidSlamDownP :: Monoid (SlamDownP a) where
  mempty = SlamDown mempty

instance arbitrarySlamDownP :: (Arbitrary a) => Arbitrary (SlamDownP a) where
  arbitrary = SlamDown <<< L.toList <$> arrayOf arbitrary

