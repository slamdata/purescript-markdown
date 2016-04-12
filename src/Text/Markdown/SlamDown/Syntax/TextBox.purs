module Text.Markdown.SlamDown.Syntax.TextBox
  ( TimeValue()
  , DateValue()
  , DateTimeValue()
  , TextBox(..)
  , transTextBox
  , traverseTextBox

  , TimeValueP()
  , DateValueP()
  , DateTimeValueP()
  ) where

import Prelude
import Data.HugeNum as HN
import Data.Identity (Identity(..), runIdentity)
import Data.NaturalTransformation (Natural)
import Test.StrongCheck as SC
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

instance showTimeValueP ∷ Show TimeValueP where
  show (TimeValueP { hours, minutes }) =
    "{ hours : "
       <> show hours
       <> ", minutes : "
       <> show minutes
       <> " }"

instance arbitraryTimeValueP ∷ SC.Arbitrary TimeValueP where
  arbitrary = do
    hours ← Gen.chooseInt 0.0 12.0
    minutes ← Gen.chooseInt 0.0 60.0
    pure $ TimeValueP { hours , minutes }

instance coarbitraryTimeValueP :: SC.CoArbitrary TimeValueP where
  coarbitrary (TimeValueP { hours, minutes }) gen = do
    SC.coarbitrary hours gen
    SC.coarbitrary minutes gen

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

instance showDateValueP ∷ Show DateValueP where
  show (DateValueP { month, day, year }) =
    "{ month : "
       <> show month
       <> ", day : "
       <> show day
       <> ", year : "
       <> show year
       <> " }"

instance arbitraryDateValueP ∷ SC.Arbitrary DateValueP where
  arbitrary = do
    month ← Gen.chooseInt 0.0 12.0
    day ← Gen.chooseInt 0.0 30.0
    year ← Gen.chooseInt 0.0 3000.0
    pure $ DateValueP { month , day, year }

instance coarbitraryDateValueP :: SC.CoArbitrary DateValueP where
  coarbitrary (DateValueP { month, day, year }) gen = do
    SC.coarbitrary month gen
    SC.coarbitrary day gen
    SC.coarbitrary year gen

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
    DateValueP v1.date == DateValueP v2.date
      && TimeValueP v1.time == TimeValueP v2.time

instance showDateTimeValueP ∷ Show DateTimeValueP where
  show (DateTimeValueP { date, time }) =
    "{ date : "
       <> show (DateValueP date)
       <> ", time : "
       <> show (TimeValueP time)
       <> " }"

instance arbitraryDateTimeValueP ∷ SC.Arbitrary DateTimeValueP where
  arbitrary = do
    DateValueP date ← SC.arbitrary
    TimeValueP time ← SC.arbitrary
    pure $ DateTimeValueP { date, time }

instance coarbitraryDateTimeValueP :: SC.CoArbitrary DateTimeValueP where
  coarbitrary (DateTimeValueP { date, time }) gen = do
    SC.coarbitrary (DateValueP date) gen
    SC.coarbitrary (TimeValueP time) gen

data TextBox f
  = PlainText (f String)
  | Numeric (f HN.HugeNum)
  | Date (f DateValue)
  | Time (f TimeValue)
  | DateTime (f DateTimeValue)

transTextBox
  ∷ ∀ f g
  . Natural f g
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
  show t =
    case t of
      PlainText def → "(PlainText " <> show def <> ")"
      Numeric def → "(Numeric " <> show def <> ")"
      Date def → "(Date " <> show (DateValueP <$> def) <> ")"
      Time def → "(Time " <> show (TimeValueP <$> def) <> ")"
      DateTime def → "(DateTime " <> show (DateTimeValueP <$> def) <> ")"

instance eqTextBox ∷ (Functor f, Eq (f String), Eq (f HN.HugeNum), Eq (f TimeValueP), Eq (f DateValueP), Eq (f DateTimeValueP)) ⇒ Eq (TextBox f) where
  eq (PlainText d1) (PlainText d2) =
    d1 == d2
  eq (Numeric d1) (Numeric d2) =
    d1 == d2
  eq (Date d1) (Date d2) =
    map DateValueP d1 == map DateValueP d2
  eq (Time d1) (Time d2) =
    map TimeValueP d1 == map TimeValueP d2
  eq (DateTime d1) (DateTime d2) =
    map DateTimeValueP d1 == map DateTimeValueP d2
  eq _ _ =
    false

instance arbitraryTextBox ∷ (Functor f, SC.Arbitrary (f String), SC.Arbitrary (f Number), SC.Arbitrary (f TimeValueP), SC.Arbitrary (f DateValueP), SC.Arbitrary (f DateTimeValueP)) ⇒ SC.Arbitrary (TextBox f) where
  arbitrary = do
    i ← Gen.chooseInt 0.0 5.0
    case i of
      0 → PlainText <$> SC.arbitrary
      1 → Numeric <<< map HN.fromNumber <$> SC.arbitrary
      2 → Date <<< map getDateValueP <$> SC.arbitrary
      3 → Time <<< map getTimeValueP <$> SC.arbitrary
      4 → DateTime <<< map getDateTimeValueP <$> SC.arbitrary
      _ → PlainText <$> SC.arbitrary

instance coarbitraryTextBox :: (Functor f, SC.CoArbitrary (f String), SC.CoArbitrary (f Number), SC.CoArbitrary (f DateValueP), SC.CoArbitrary (f TimeValueP), SC.CoArbitrary (f DateTimeValueP)) ⇒ SC.CoArbitrary (TextBox f) where
  coarbitrary t =
    case t of
      PlainText d -> SC.coarbitrary d
      Numeric d -> SC.coarbitrary $ HN.toNumber <$> d
      Date d -> SC.coarbitrary $ DateValueP <$> d
      Time d -> SC.coarbitrary $ TimeValueP <$> d
      DateTime d -> SC.coarbitrary $ DateTimeValueP <$> d
