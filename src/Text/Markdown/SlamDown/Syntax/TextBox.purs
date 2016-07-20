module Text.Markdown.SlamDown.Syntax.TextBox
  ( TimeValue
  , DateValue
  , DateTimeValue
  , TextBox(..)
  , transTextBox
  , traverseTextBox

  , TimeValueP
  , DateValueP
  , DateTimeValueP
  ) where

import Prelude

import Data.Function (on)
import Data.HugeNum as HN
import Data.Identity (Identity(..), runIdentity)

import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen

type TimeValue =
  { hours ∷ Int
  , minutes ∷ Int
  }

newtype TimeValueP = TimeValueP TimeValue

getTimeValueP
  ∷ TimeValueP
  → TimeValue
getTimeValueP (TimeValueP v) =
  v

instance eqTimeValueP ∷ Eq TimeValueP where
  eq (TimeValueP v1) (TimeValueP v2) =
    v1.hours == v2.hours
      && v1.minutes == v2.minutes

instance ordTimeValueP ∷ Ord TimeValueP where
  compare (TimeValueP v1) (TimeValueP v2) =
    compare v1.hours v2.hours
      <> compare v1.minutes v2.minutes

instance showTimeValueP ∷ Show TimeValueP where
  show (TimeValueP { hours, minutes }) =
    "{ hours : "
       <> show hours
       <> ", minutes : "
       <> show minutes
       <> " }"

instance arbitraryTimeValueP ∷ SCA.Arbitrary TimeValueP where
  arbitrary = do
    hours ← Gen.chooseInt 0.0 12.0
    minutes ← Gen.chooseInt 0.0 60.0
    pure $ TimeValueP { hours , minutes }

instance coarbitraryTimeValueP :: SCA.Coarbitrary TimeValueP where
  coarbitrary (TimeValueP { hours, minutes }) gen = do
    SCA.coarbitrary hours gen
    SCA.coarbitrary minutes gen

type DateValue =
  { month ∷ Int
  , day ∷ Int
  , year ∷ Int
  }

newtype DateValueP = DateValueP DateValue

getDateValueP
  ∷ DateValueP
  → DateValue
getDateValueP (DateValueP v) =
  v

instance eqDateValueP ∷ Eq DateValueP where
  eq (DateValueP v1) (DateValueP v2) =
    v1.month == v2.month
      && v1.day == v2.day
      && v1.year == v2.year

instance ordDateValueP ∷ Ord DateValueP where
  compare (DateValueP v1) (DateValueP v2) =
    compare v1.year v2.year
      <> compare v1.month v2.month
      <> compare v1.day v2.day

instance showDateValueP ∷ Show DateValueP where
  show (DateValueP { month, day, year }) =
    "{ month : "
       <> show month
       <> ", day : "
       <> show day
       <> ", year : "
       <> show year
       <> " }"

instance arbitraryDateValueP ∷ SCA.Arbitrary DateValueP where
  arbitrary = do
    month ← Gen.chooseInt 0.0 12.0
    day ← Gen.chooseInt 0.0 30.0
    year ← Gen.chooseInt 0.0 3000.0
    pure $ DateValueP { month , day, year }

instance coarbitraryDateValueP :: SCA.Coarbitrary DateValueP where
  coarbitrary (DateValueP { month, day, year }) gen = do
    SCA.coarbitrary month gen
    SCA.coarbitrary day gen
    SCA.coarbitrary year gen

type DateTimeValue =
  { date ∷ DateValue
  , time ∷ TimeValue
  }

newtype DateTimeValueP = DateTimeValueP DateTimeValue

getDateTimeValueP
  ∷ DateTimeValueP
  → DateTimeValue
getDateTimeValueP (DateTimeValueP v) =
  v

instance eqDateTimeValueP ∷ Eq DateTimeValueP where
  eq (DateTimeValueP v1) (DateTimeValueP v2) =
    on eq (DateValueP <<< _.date) v1 v2
      && on eq (TimeValueP <<< _.time) v1 v2

instance ordDateTimeValueP ∷ Ord DateTimeValueP where
  compare (DateTimeValueP v1) (DateTimeValueP v2) =
    on compare (DateValueP <<< _.date) v1 v2
      <> on compare (TimeValueP <<< _.time) v1 v2

instance showDateTimeValueP ∷ Show DateTimeValueP where
  show (DateTimeValueP { date, time }) =
    "{ date : "
       <> show (DateValueP date)
       <> ", time : "
       <> show (TimeValueP time)
       <> " }"

instance arbitraryDateTimeValueP ∷ SCA.Arbitrary DateTimeValueP where
  arbitrary = do
    DateValueP date ← SCA.arbitrary
    TimeValueP time ← SCA.arbitrary
    pure $ DateTimeValueP { date, time }

instance coarbitraryDateTimeValueP :: SCA.Coarbitrary DateTimeValueP where
  coarbitrary (DateTimeValueP { date, time }) gen = do
    SCA.coarbitrary (DateValueP date) gen
    SCA.coarbitrary (TimeValueP time) gen

data TextBox f
  = PlainText (f String)
  | Numeric (f HN.HugeNum)
  | Date (f DateValue)
  | Time (f TimeValue)
  | DateTime (f DateTimeValue)

transTextBox
  ∷ ∀ f g
  . (f ~> g)
  → TextBox f
  → TextBox g
transTextBox eta =
  runIdentity <<<
    traverseTextBox (eta >>> Identity)

traverseTextBox
  ∷ ∀ f g h
  . (Applicative h)
  ⇒ (∀ a. f a → h (g a))
  → TextBox f
  → h (TextBox g)
traverseTextBox eta t =
  case t of
    PlainText def → PlainText <$> eta def
    Numeric def → Numeric <$> eta def
    Date def → Date <$> eta def
    Time def → Time <$> eta def
    DateTime def → DateTime <$> eta def

instance showTextBox ∷ (Functor f, Show (f String), Show (f HN.HugeNum), Show (f TimeValueP), Show (f DateValueP), Show (f DateTimeValueP)) ⇒ Show (TextBox f) where
  show =
    case _ of
      PlainText def → "(PlainText " <> show def <> ")"
      Numeric def → "(Numeric " <> show def <> ")"
      Date def → "(Date " <> show (DateValueP <$> def) <> ")"
      Time def → "(Time " <> show (TimeValueP <$> def) <> ")"
      DateTime def → "(DateTime " <> show (DateTimeValueP <$> def) <> ")"

instance ordTextBox ∷ (Functor f, Ord (f String), Ord (f HN.HugeNum), Ord (f TimeValueP), Ord (f DateValueP), Ord (f DateTimeValueP)) ⇒ Ord (TextBox f) where
  compare =
    case _, _ of
      PlainText d1, PlainText d2 → compare d1 d2
      PlainText _, _ → LT
      _, PlainText _ → GT

      Numeric d1, Numeric d2 → compare d1 d2
      Numeric _, _ → LT
      _, Numeric _ → GT

      Date d1, Date d2 → on compare (map DateValueP) d1 d2
      Date _, _ → LT
      _, Date _ → GT

      Time t1, Time t2 → on compare (map TimeValueP) t1 t2
      Time _, _ → LT
      _, Time _ → GT

      DateTime d1, DateTime d2 → on compare (map DateTimeValueP) d1 d2

instance eqTextBox ∷ (Functor f, Eq (f String), Eq (f HN.HugeNum), Eq (f TimeValueP), Eq (f DateValueP), Eq (f DateTimeValueP)) ⇒ Eq (TextBox f) where
  eq =
    case _, _ of
      PlainText d1, PlainText d2 → d1 == d2
      Numeric d1, Numeric d2 → d1 == d2
      Date d1, Date d2 → on eq (map DateValueP) d1 d2
      Time d1, Time d2 → on eq (map TimeValueP) d1 d2
      DateTime d1, DateTime d2 → on eq (map DateTimeValueP) d1 d2
      _, _ → false

instance arbitraryTextBox ∷ (Functor f, SCA.Arbitrary (f String), SCA.Arbitrary (f Number), SCA.Arbitrary (f TimeValueP), SCA.Arbitrary (f DateValueP), SCA.Arbitrary (f DateTimeValueP)) ⇒ SCA.Arbitrary (TextBox f) where
  arbitrary = do
    i ← Gen.chooseInt 0.0 5.0
    case i of
      0 → PlainText <$> SCA.arbitrary
      1 → Numeric <<< map HN.fromNumber <$> SCA.arbitrary
      2 → Date <<< map getDateValueP <$> SCA.arbitrary
      3 → Time <<< map getTimeValueP <$> SCA.arbitrary
      4 → DateTime <<< map getDateTimeValueP <$> SCA.arbitrary
      _ → PlainText <$> SCA.arbitrary

instance coarbitraryTextBox :: (Functor f, SCA.Coarbitrary (f String), SCA.Coarbitrary (f Number), SCA.Coarbitrary (f DateValueP), SCA.Coarbitrary (f TimeValueP), SCA.Coarbitrary (f DateTimeValueP)) ⇒ SCA.Coarbitrary (TextBox f) where
  coarbitrary =
    case _ of
      PlainText d -> SCA.coarbitrary d
      Numeric d -> SCA.coarbitrary $ HN.toNumber <$> d
      Date d -> SCA.coarbitrary $ DateValueP <$> d
      Time d -> SCA.coarbitrary $ TimeValueP <$> d
      DateTime d -> SCA.coarbitrary $ DateTimeValueP <$> d
