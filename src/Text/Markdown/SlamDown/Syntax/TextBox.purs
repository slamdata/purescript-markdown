module Text.Markdown.SlamDown.Syntax.TextBox
  ( TimePrecision(..)
  , TextBox(..)
  , transTextBox
  , traverseTextBox
  ) where

import Prelude

import Data.DateTime as DT
import Data.Eq (class Eq1)
import Data.HugeNum as HN
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Data.Ord (class Ord1)

data TimePrecision
  = Minutes
  | Seconds

derive instance eqTimePrecision ∷ Eq TimePrecision
derive instance ordTimePrecision ∷ Ord TimePrecision

instance showTimePrecision ∷ Show TimePrecision where
  show Minutes = "Minutes"
  show Seconds = "Seconds"

data TextBox f
  = PlainText (f String)
  | Numeric (f HN.HugeNum)
  | Date (f DT.Date)
  | Time TimePrecision (f DT.Time)
  | DateTime TimePrecision (f DT.DateTime)

transTextBox ∷ ∀ f g. (f ~> g) → TextBox f → TextBox g
transTextBox eta = unwrap <<< traverseTextBox (Identity <<< eta)

traverseTextBox
  ∷ ∀ f g h
  . Applicative h
  ⇒ (∀ a. f a → h (g a))
  → TextBox f
  → h (TextBox g)
traverseTextBox eta = case _ of
  PlainText def → PlainText <$> eta def
  Numeric def → Numeric <$> eta def
  Date def → Date <$> eta def
  Time prec def → Time prec <$> eta def
  DateTime prec def → DateTime prec <$> eta def

instance showTextBox ∷ (Show (f String), Show (f HN.HugeNum), Show (f DT.Time), Show (f DT.Date), Show (f DT.DateTime)) ⇒ Show (TextBox f) where
  show = case _ of
    PlainText def → "(PlainText " <> show def <> ")"
    Numeric def → "(Numeric " <> show def <> ")"
    Date def → "(Date " <> show def <> ")"
    Time prec def → "(Time " <> show prec <> " " <> show def <> ")"
    DateTime prec def → "(DateTime " <> show prec <> " " <> show def <> ")"

derive instance eqTextBox ∷ Eq1 f ⇒ Eq (TextBox f)
derive instance ordTextBox ∷ Ord1 f ⇒ Ord (TextBox f)

eraseMillis ∷ DT.Time → DT.Time
eraseMillis (DT.Time h m s _) = DT.Time h m s bottom
