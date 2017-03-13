module Text.Markdown.SlamDown.Syntax.TextBox
  ( TimePrecision(..)
  , TextBox(..)
  , transTextBox
  , traverseTextBox
  ) where

import Prelude

import Data.DateTime as DT
import Data.HugeNum as HN
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)

import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Data.ArbDateTime as ADT
import Test.StrongCheck.Gen as Gen

data TimePrecision
  = Minutes
  | Seconds

derive instance eqTimePrecision ∷ Eq TimePrecision
derive instance ordTimePrecision ∷ Ord TimePrecision

instance showTimePrecision ∷ Show TimePrecision where
  show Minutes = "Minutes"
  show Seconds = "Seconds"

instance arbitraryTimePrecision ∷ SCA.Arbitrary TimePrecision where
  arbitrary =
    Gen.chooseInt 0 1 <#> case _ of
      0 → Minutes
      _ → Seconds

instance coarbitraryTimePrecision ∷ SCA.Coarbitrary TimePrecision where
  coarbitrary Minutes = SCA.coarbitrary 1
  coarbitrary Seconds = SCA.coarbitrary 2

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

instance showTextBox ∷ (Functor f, Show (f String), Show (f HN.HugeNum), Show (f DT.Time), Show (f DT.Date), Show (f DT.DateTime)) ⇒ Show (TextBox f) where
  show = case _ of
    PlainText def → "(PlainText " <> show def <> ")"
    Numeric def → "(Numeric " <> show def <> ")"
    Date def → "(Date " <> show def <> ")"
    Time prec def → "(Time " <> show prec <> " " <> show def <> ")"
    DateTime prec def → "(DateTime " <> show prec <> " " <> show def <> ")"

derive instance eqTextBox ∷ (Functor f, Eq (f String), Eq (f HN.HugeNum), Eq (f DT.Time), Eq (f DT.Date), Eq (f DT.DateTime)) ⇒ Eq (TextBox f)
derive instance ordTextBox ∷ (Functor f, Ord (f String), Ord (f HN.HugeNum), Ord (f DT.Time), Ord (f DT.Date), Ord (f DT.DateTime)) ⇒ Ord (TextBox f)

instance arbitraryTextBox ∷ (Functor f, SCA.Arbitrary (f String), SCA.Arbitrary (f Number), SCA.Arbitrary (f ADT.ArbTime), SCA.Arbitrary (f ADT.ArbDate), SCA.Arbitrary (f ADT.ArbDateTime)) ⇒ SCA.Arbitrary (TextBox f) where
  arbitrary = do
    i ← Gen.chooseInt 0 5
    case i of
      0 → PlainText <$> SCA.arbitrary
      1 → Numeric <<< map HN.fromNumber <$> SCA.arbitrary
      2 → Date <<< map ADT.runArbDate <$> SCA.arbitrary
      3 → Time <$> SCA.arbitrary <*> (map (eraseMillis <<< ADT.runArbTime) <$> SCA.arbitrary)
      4 → DateTime <$> SCA.arbitrary <*> (map (DT.modifyTime eraseMillis <<< ADT.runArbDateTime) <$> SCA.arbitrary)
      _ → PlainText <$> SCA.arbitrary

instance coarbitraryTextBox ∷ (Functor f, SCA.Coarbitrary (f String), SCA.Coarbitrary (f Number), SCA.Coarbitrary (f ADT.ArbDate), SCA.Coarbitrary (f ADT.ArbTime), SCA.Coarbitrary (f ADT.ArbDateTime)) ⇒ SCA.Coarbitrary (TextBox f) where
  coarbitrary =
    case _ of
      PlainText d -> SCA.coarbitrary d
      Numeric d -> SCA.coarbitrary $ HN.toNumber <$> d
      Date d -> SCA.coarbitrary (ADT.ArbDate <$> d)
      Time prec d -> do
        SCA.coarbitrary prec
        SCA.coarbitrary (ADT.ArbTime <$> d)
      DateTime prec d -> do
        SCA.coarbitrary prec
        SCA.coarbitrary (ADT.ArbDateTime <$> d)

eraseMillis ∷ DT.Time → DT.Time
eraseMillis (DT.Time h m s _) = DT.Time h m s bottom
