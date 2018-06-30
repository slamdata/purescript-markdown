module Text.Markdown.SlamDown.Syntax.FormField
  ( FormFieldP(..)
  , FormField
  , transFormField
  , traverseFormField

  , Expr(..)
  , getLiteral
  , getUnevaluated

  , module Value
  , module TB
  ) where

import Prelude

import Data.Eq (class Eq1, eq1)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.List as L
import Data.Maybe as M
import Data.Newtype (unwrap)
import Data.Ord (class Ord1, compare1)
import Data.Traversable as TR
import Text.Markdown.SlamDown.Syntax.TextBox (TextBox(..), TimePrecision(..), transTextBox, traverseTextBox) as TB
import Text.Markdown.SlamDown.Syntax.Value (class Value, renderValue, stringValue) as Value

data FormFieldP f a
  = TextBox (TB.TextBox (Compose M.Maybe f))
  | RadioButtons (f a) (f (L.List a))
  | CheckBoxes (f (L.List a)) (f (L.List a))
  | DropDown (M.Maybe (f a)) (f (L.List a))

type FormField a = FormFieldP Expr a

transFormField
  ∷ ∀ f g
  . (f ~> g)
  → FormFieldP f
  ~> FormFieldP g
transFormField eta =
  unwrap <<<
    traverseFormField (eta >>> Identity)

traverseFormField
  ∷ ∀ f g h a
  . Applicative h
  ⇒ (∀ x. f x → h (g x))
  → FormFieldP f a
  → h (FormFieldP g a)
traverseFormField eta field =
  case field of
    TextBox tb → TextBox <$> TB.traverseTextBox (unwrap >>> TR.traverse eta >>> map Compose) tb
    RadioButtons sel ls → RadioButtons <$> eta sel <*> eta ls
    CheckBoxes sel ls → CheckBoxes <$> eta sel <*> eta ls
    DropDown sel ls → DropDown <$> TR.traverse eta sel <*> eta ls

instance functorFormField ∷ (Functor f) ⇒ Functor (FormFieldP f) where
  map f x =
    case x of
      TextBox tb → TextBox tb
      RadioButtons sel ls → RadioButtons (f <$> sel) (map f <$> ls)
      CheckBoxes sel ls → CheckBoxes (map f <$> sel) (map f <$> ls)
      DropDown sel ls → DropDown (map f <$> sel) (map f <$> ls)

instance showFormField ∷ (Functor f, Show (f a), Show (TB.TextBox (Compose M.Maybe f)), Show (f (L.List a))) ⇒ Show (FormFieldP f a) where
  show =
    case _ of
      TextBox tb → "(TextBox " <> show tb <> ")"
      RadioButtons sel ls → "(RadioButtons " <> show sel <> " " <> show ls <> ")"
      CheckBoxes sel ls → "(CheckBoxes " <> show sel <> " " <> show ls <> ")"
      DropDown sel ls → "(DropDown " <> show sel <> " " <> show ls <> ")"

instance eq1FormField ∷ Eq1 f ⇒ Eq1 (FormFieldP f) where
  eq1 = case _, _ of
    TextBox tb1, TextBox tb2 -> tb1 == tb2
    RadioButtons sel1 ls1, RadioButtons sel2 ls2 -> sel1 `eq1` sel2 && ls1 `eq1` ls2
    CheckBoxes sel1 ls1, CheckBoxes sel2 ls2 -> sel1 `eq1` sel2 && ls1 `eq1` ls2
    DropDown M.Nothing ls1, DropDown M.Nothing ls2 -> ls1 `eq1` ls2
    DropDown (M.Just sel1) ls1, DropDown (M.Just sel2) ls2 -> sel1 `eq1` sel2 && ls1 `eq1` ls2
    _, _ -> false

instance eqFormField :: (Eq1 f, Eq a) => Eq (FormFieldP f a) where
  eq = eq1

instance ord1FormField ∷ Ord1 f ⇒ Ord1 (FormFieldP f) where
  compare1 =
    case _, _ of
      TextBox tb1, TextBox tb2 → compare tb1 tb2
      TextBox _, _ → LT
      _, TextBox _ → GT

      RadioButtons sel1 ls1, RadioButtons sel2 ls2 → compare1 sel1 sel2 <> compare1 ls1 ls2
      RadioButtons _ _, _ → LT
      _, RadioButtons _ _ → GT

      CheckBoxes sel1 ls1, CheckBoxes sel2 ls2 → compare1 sel1 sel2 <> compare1 ls1 ls2
      CheckBoxes _ _, _ → LT
      _, CheckBoxes _ _ → GT

      DropDown M.Nothing ls1, DropDown M.Nothing ls2 → compare1 ls1 ls2
      DropDown (M.Just sel1) ls1, DropDown (M.Just sel2) ls2 → compare1 sel1 sel2 <> compare1 ls1 ls2
      _, _ -> EQ

instance ordFormField :: (Ord1 f, Ord a) => Ord (FormFieldP f a) where
  compare = compare1

newtype ArbIdentity a = ArbIdentity a

getArbIdentity
  ∷ ∀ a
  . ArbIdentity a
  → Identity a
getArbIdentity (ArbIdentity x) =
  Identity x

instance functorArbIdentity ∷ Functor ArbIdentity where
  map f (ArbIdentity x) =
    ArbIdentity $ f x

newtype ArbCompose f g a = ArbCompose (f (g a))

getArbCompose
  ∷ ∀ f g a
  . ArbCompose f g a
  → Compose f g a
getArbCompose (ArbCompose x) =
  Compose x

instance functorArbCompose ∷ (Functor f, Functor g) ⇒ Functor (ArbCompose f g) where
  map f (ArbCompose x) =
    ArbCompose $
      map (map f) x

data Expr a
  = Literal a
  | Unevaluated String

getUnevaluated ∷ ∀ e. Expr e → M.Maybe String
getUnevaluated (Unevaluated s) = M.Just s
getUnevaluated _ = M.Nothing

getLiteral ∷ ∀ e. Expr e → M.Maybe e
getLiteral (Literal e) = M.Just e
getLiteral _ = M.Nothing

instance functorExpr ∷ Functor Expr where
  map f =
    case _ of
      Literal a → Literal $ f a
      Unevaluated e → Unevaluated e

instance showExpr ∷ (Show a) ⇒ Show (Expr a) where
  show =
    case _ of
      Literal a → "(Literal " <> show a <> ")"
      Unevaluated e → "(Unevaluated " <> show e <> ")"

derive instance eqExpr ∷ Eq a ⇒ Eq (Expr a)
derive instance eq1 ∷ Eq1 Expr

derive instance ord1Expr ∷ Ord1 Expr
derive instance ordExpr ∷ Ord a ⇒ Ord (Expr a)
