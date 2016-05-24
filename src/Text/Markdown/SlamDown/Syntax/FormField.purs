module Text.Markdown.SlamDown.Syntax.FormField
  ( FormFieldP(..)
  , FormField()
  , transFormField
  , traverseFormField

  , Expr(..)
  , getLiteral
  , getUnevaluated

  , module Value
  , module TB
  ) where

import Prelude
import Data.Array as A
import Data.Identity (Identity(..), runIdentity)
import Data.Functor.Compose (Compose(..), decompose)
import Data.NaturalTransformation (Natural)
import Data.List as L
import Data.Maybe as M
import Data.Set as Set
import Data.Traversable as TR
import Data.Tuple (uncurry)

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

import Text.Markdown.SlamDown.Syntax.Value as Value
import Text.Markdown.SlamDown.Syntax.TextBox as TB

data FormFieldP f a
  = TextBox (TB.TextBox (Compose M.Maybe f))
  | RadioButtons (f a) (f (L.List a))
  | CheckBoxes (f (L.List a)) (f (L.List a))
  | DropDown (M.Maybe (f a)) (f (L.List a))

type FormField a = FormFieldP Expr a

transFormField
  ∷ ∀ f g
  . (Natural f g)
  → Natural (FormFieldP f) (FormFieldP g)
transFormField eta =
  runIdentity <<<
    traverseFormField (eta >>> Identity)

traverseFormField
  ∷ ∀ f g h a
  . (Applicative h)
  ⇒ (∀ x. f x → h (g x))
  → FormFieldP f a
  → h (FormFieldP g a)
traverseFormField eta field =
  case field of
    TextBox tb → TextBox <$> TB.traverseTextBox (decompose >>> TR.traverse eta >>> map Compose) tb
    RadioButtons sel ls → RadioButtons <$> eta sel <*> eta ls
    CheckBoxes sel ls → CheckBoxes <$> eta ls <*> eta ls
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
      TextBox tb → "(TextBox " ++ show tb ++ ")"
      RadioButtons sel ls → "(RadioButtons " ++ show sel ++ " " ++ show ls ++ ")"
      CheckBoxes sel ls → "(CheckBoxes " ++ show sel ++ " " ++ show ls ++ ")"
      DropDown sel ls → "(DropDown " ++ show sel ++ " " ++ show ls ++ ")"

instance ordFormField ∷ (Functor f, Ord (f a), Ord (TB.TextBox (Compose M.Maybe f)), Eq (TB.TextBox (Compose M.Maybe f)), Ord (f (L.List a)), Ord (f (Set.Set a)), Ord a) ⇒ Ord (FormFieldP f a) where
  compare =
    case _, _ of
      TextBox tb1, TextBox tb2 → compare tb1 tb2
      TextBox _, _ → LT
      _, TextBox _ → GT

      RadioButtons sel1 ls1, RadioButtons sel2 ls2 → compare sel1 sel2 <> compare ls1 ls2
      RadioButtons _ _, _ → LT
      _, RadioButtons _ _ → GT

      CheckBoxes sel1 ls1, CheckBoxes sel2 ls2 → compare (Set.fromList <$> sel1) (Set.fromList <$> sel2) <> compare ls1 ls2
      CheckBoxes _ _, _ → LT
      _, CheckBoxes _ _ → GT

      DropDown sel1 ls1, DropDown sel2 ls2 → compare sel1 sel2 <> compare ls1 ls2

instance eqFormField ∷ (Functor f, Eq (f a), Eq (TB.TextBox (Compose M.Maybe f)), Eq (f (L.List a)), Eq (f (Set.Set a)), Ord a) ⇒ Eq (FormFieldP f a) where
  eq =
    case _, _ of
      TextBox tb1, TextBox tb2 → tb1 == tb2
      RadioButtons sel1 ls1, RadioButtons sel2 ls2 → sel1 == sel2 && ls1 == ls2
      CheckBoxes sel1 ls1, CheckBoxes sel2 ls2 → (Set.fromList <$> sel1 == Set.fromList <$> sel2) && ls1 == ls2
      DropDown sel1 ls1, DropDown sel2 ls2 → sel1 == sel2 && ls1 == ls2
      _, _ → false

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

instance arbitraryArbIdentity ∷ (SC.Arbitrary a) ⇒ SC.Arbitrary (ArbIdentity a) where
  arbitrary =
    ArbIdentity <$>
      SC.arbitrary

instance coarbitraryArbIdentity ∷ (SC.CoArbitrary a) ⇒ SC.CoArbitrary (ArbIdentity a) where
  coarbitrary (ArbIdentity x) =
    SC.coarbitrary x

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

instance arbitraryArbCompose ∷ (SC.Arbitrary (f (g a))) ⇒ SC.Arbitrary (ArbCompose f g a) where
  arbitrary =
    ArbCompose <$>
      SC.arbitrary

instance coarbitraryArbCompose ∷ (SC.CoArbitrary (f (g a))) ⇒ SC.CoArbitrary (ArbCompose f g a) where
  coarbitrary (ArbCompose t) =
    SC.coarbitrary t

unsafeElements ∷ ∀ a. L.List a → Gen.Gen a
unsafeElements =
  Gen.elements
    <$> (Data.Maybe.Unsafe.fromJust <<< L.head)
    <*> id

instance arbitraryFormField ∷ (SC.Arbitrary a, Eq a) ⇒ SC.Arbitrary (FormFieldP Expr a) where
  arbitrary = do
    k ← Gen.chooseInt 0.0 3.0
    case k of
      0 → TextBox <<< TB.transTextBox getArbCompose <$> SC.arbitrary
      1 → do
        xse ← genExpr $ distinctListOf1 SC.arbitrary
        case xse of
          Literal xs → do
            x ← Literal <$> unsafeElements xs
            pure $ RadioButtons x xse
          Unevaluated e → do
            x ← Unevaluated <$> genUnevaluated
            pure $ RadioButtons x xse
      2 → do
        xse ← genExpr $ distinctListOf1 SC.arbitrary
        case xse of
          Literal xs → do
            ys ← Literal <$> distinctListOf (unsafeElements xs)
            pure $ CheckBoxes ys xse
          Unevaluated e → do
            yse ← Unevaluated <$> genUnevaluated
            pure $ CheckBoxes yse xse
      _ → do
        xse ← genExpr $ distinctListOf1 SC.arbitrary
        case xse of
          Literal xs → do
            mx ← genMaybe $ Literal <$> unsafeElements xs
            pure $ DropDown mx xse
          Unevaluated e → do
            mx ← genMaybe $ Unevaluated <$> genUnevaluated
            pure $ DropDown mx xse

instance arbitraryFormFieldIdentity ∷ (SC.Arbitrary a, Eq a) ⇒ SC.Arbitrary (FormFieldP Identity a) where
  arbitrary = do
    k ← Gen.chooseInt 0.0 3.0
    case k of
      0 → TextBox <<< TB.transTextBox (\(ArbCompose x) → Compose $ map getArbIdentity x) <$> SC.arbitrary
      1 → do
        xs ← distinctListOf1 $ getArbIdentity <$> SC.arbitrary
        x ← unsafeElements xs
        pure $ RadioButtons x $ TR.sequence xs
      2 → do
        xs ← map TR.sequence <<< distinctListOf1 $ getArbIdentity <$> SC.arbitrary
        ys ← TR.traverse (distinctListOf <<< unsafeElements) xs
        pure $ CheckBoxes ys xs
      _ → do
        xs ← distinctListOf1 $ getArbIdentity <$> SC.arbitrary
        mx ← genMaybe $ unsafeElements xs
        pure $ DropDown mx $ TR.sequence xs


genMaybe
  ∷ ∀ a
  . Gen.Gen a
  → Gen.Gen (M.Maybe a)
genMaybe gen = do
  b ← SC.arbitrary
  if b then M.Just <$> gen else pure M.Nothing

instance coarbitraryFormFieldIdentity ∷ (SC.CoArbitrary a) ⇒ SC.CoArbitrary (FormFieldP Identity a) where
  coarbitrary field =
    case field of
      TextBox tb → SC.coarbitrary $ TB.transTextBox (decompose >>> map (runIdentity >>> ArbIdentity) >>> ArbCompose) tb
      RadioButtons x xs → \gen → do
        SC.coarbitrary (ArbIdentity $ runIdentity x) gen
        SC.coarbitrary (ArbIdentity $ runIdentity xs) gen
      CheckBoxes sel xs → \gen → do
        SC.coarbitrary (ArbIdentity $ runIdentity sel) gen
        SC.coarbitrary (ArbIdentity $ runIdentity xs) gen
      DropDown mx xs → \gen → do
        SC.coarbitrary (ArbIdentity <<< runIdentity <$> mx) gen
        SC.coarbitrary (ArbIdentity $ runIdentity xs) gen

listOf1
  ∷ ∀ f a
  . (Monad f)
  ⇒ Gen.GenT f a
  → Gen.GenT f (L.List a)
listOf1 =
  map (L.toList <<< uncurry A.cons)
    <<< Gen.arrayOf1

listOf
  ∷ ∀ f a
  . (Monad f)
  ⇒ Gen.GenT f a
  → Gen.GenT f (L.List a)
listOf =
  map L.toList
    <<< Gen.arrayOf

listOfLength
  ∷ ∀ f a
  . (Monad f)
  ⇒ Int
  → Gen.GenT f a
  → Gen.GenT f (L.List a)
listOfLength i =
  map L.toList
    <<< Gen.vectorOf i

distinctListOf1
  ∷ ∀ f a
  . (Monad f, Eq a)
  ⇒ Gen.GenT f a
  → Gen.GenT f (L.List a)
distinctListOf1 =
  map (map L.nub) listOf1

distinctListOf
  ∷ ∀ f a
  . (Monad f, Eq a)
  ⇒ Gen.GenT f a
  → Gen.GenT f (L.List a)
distinctListOf =
  map (map L.nub) listOf

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
      Literal a → "(Literal " ++ show a ++ ")"
      Unevaluated e → "(Unevaluated " ++ show e ++ ")"

derive instance eqExpr ∷ Eq a ⇒ Eq (Expr a)
derive instance ordExpr ∷ Ord a ⇒ Ord (Expr a)

genExpr ∷ ∀ a. Gen.Gen a → Gen.Gen (Expr a)
genExpr g = do
  b ← SC.arbitrary
  if b then Literal <$> g else Unevaluated <$> genUnevaluated

genUnevaluated ∷ Gen.Gen String
genUnevaluated = do
  x ← SC.arbitrary
  pure $ " " <> x <> " "

instance arbitraryExpr ∷ (SC.Arbitrary a) ⇒ SC.Arbitrary (Expr a) where
  arbitrary = genExpr SC.arbitrary
