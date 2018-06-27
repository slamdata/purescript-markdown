module Text.Markdown.SlamDown.Syntax
  ( SlamDownP(..)
  , SlamDown

  , module SDF
  , module SDI
  , module SDB
  ) where

import Prelude

import Data.List as L
import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen

import Text.Markdown.SlamDown.Syntax.Block (Block(..), CodeBlockType(..), ListType(..)) as SDB
import Text.Markdown.SlamDown.Syntax.FormField (class Value, Expr(..), FormField, FormFieldP(..), TextBox(..), TimePrecision(..), getLiteral, getUnevaluated, renderValue, stringValue, transFormField, transTextBox, traverseFormField, traverseTextBox) as SDF
import Text.Markdown.SlamDown.Syntax.Inline (Inline(..), LinkTarget(..)) as SDI

-- | `SlamDownP` is the type of SlamDown abstract syntax trees which take values in `a`.
data SlamDownP a = SlamDown (L.List (SDB.Block a))

type SlamDown = SlamDownP String

instance functorSlamDownP ∷ Functor SlamDownP where
  map f (SlamDown bs) = SlamDown (map f <$> bs)

instance showSlamDownP ∷ (Show a) ⇒ Show (SlamDownP a) where
  show (SlamDown bs) = "(SlamDown " <> show bs <> ")"

derive instance eqSlamDownP ∷ (Eq a, Ord a) ⇒ Eq (SlamDownP a)
derive instance ordSlamDownP ∷ (Eq a, Ord a) ⇒ Ord (SlamDownP a)

instance semigroupSlamDownP ∷ Semigroup (SlamDownP a) where
  append (SlamDown bs1) (SlamDown bs2) = SlamDown (bs1 <> bs2)

instance monoidSlamDownP ∷ Monoid (SlamDownP a) where
  mempty = SlamDown mempty

instance arbitrarySlamDownP ∷ (SCA.Arbitrary a, Eq a) ⇒ SCA.Arbitrary (SlamDownP a) where
  arbitrary = SlamDown <<< L.fromFoldable <$> Gen.arrayOf SCA.arbitrary
