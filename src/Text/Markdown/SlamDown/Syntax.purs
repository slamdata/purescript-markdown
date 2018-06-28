module Text.Markdown.SlamDown.Syntax
  ( SlamDownP(..)
  , SlamDown

  , module SDF
  , module SDI
  , module SDB
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.List as L
import Data.Ord (class Ord1)
import Text.Markdown.SlamDown.Syntax.Block (Block(..), CodeBlockType(..), ListType(..)) as SDB
import Text.Markdown.SlamDown.Syntax.FormField (class Value, Expr(..), FormField, FormFieldP(..), TextBox(..), TimePrecision(..), getLiteral, getUnevaluated, renderValue, stringValue, transFormField, transTextBox, traverseFormField, traverseTextBox) as SDF
import Text.Markdown.SlamDown.Syntax.Inline (Inline(..), LinkTarget(..)) as SDI

-- | `SlamDownP` is the type of SlamDown abstract syntax trees which take values in `a`.
newtype SlamDownP a = SlamDown (L.List (SDB.Block a))

type SlamDown = SlamDownP String

derive instance functorSlamDownP ∷ Functor SlamDownP

instance showSlamDownP ∷ (Show a) ⇒ Show (SlamDownP a) where
  show (SlamDown bs) = "(SlamDown " <> show bs <> ")"

derive newtype instance eqSlamDownP ∷ Eq a ⇒ Eq (SlamDownP a)
derive instance eq1SlamDownP ∷ Eq1 SlamDownP

derive newtype instance ordSlamDownP ∷ Ord a ⇒ Ord (SlamDownP a)
derive instance ord1SlamDownP ∷ Ord1 SlamDownP

derive newtype instance semigroupSlamDownP ∷ Semigroup (SlamDownP a)
derive newtype instance monoidSlamDownP ∷ Monoid (SlamDownP a)
