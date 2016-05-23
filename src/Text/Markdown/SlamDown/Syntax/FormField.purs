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
import Data.Int as Int
import Data.List as L
import Data.Maybe as M
import Data.HugeNum as HN
import Data.Traversable as TR
import Data.Tuple (uncurry)

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

import Text.Markdown.SlamDown.Syntax.Value as Value
import Text.Markdown.SlamDown.Syntax.TextBox as TB

data FormFieldP f a
  = TextBox (TB.TextBox (Compose M.Maybe f))
  | RadioButtons (f Int) (f (L.List a))
  | CheckBoxes (f (L.List Boolean)) (f (L.List a))
  | DropDown (M.Maybe (f Int)) (f (L.List a))

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
    CheckBoxes bs ls → CheckBoxes <$> eta bs <*> eta ls
    DropDown sel ls → DropDown <$> TR.traverse eta sel <*> eta ls

instance functorFormField ∷ (Functor f) ⇒ Functor (FormFieldP f) where
  map f x =
    case x of
      TextBox tb → TextBox tb
      RadioButtons sel ls → RadioButtons sel (map f <$> ls)
      CheckBoxes bs ls → CheckBoxes bs (map f <$> ls)
      DropDown sel ls → DropDown sel (map f <$> ls)

instance showFormField ∷ (Functor f, Show (f a), Show (f Int), Show (f (L.List a)), Show (f (L.List Boolean)), Show (f String), Show (f HN.HugeNum), Show (f TB.TimeValueP), Show (f TB.DateValueP), Show (f TB.DateTimeValueP), Show a) ⇒ Show (FormFieldP f a) where
  show (TextBox tb) = "(TextBox " ++ show tb ++ ")"
  show (RadioButtons sel ls) = "(RadioButtons " ++ show sel ++ " " ++ show ls ++ ")"
  show (CheckBoxes bs ls) = "(CheckBoxes " ++ show bs ++ " " ++ show ls ++ ")"
  show (DropDown sel ls) = "(DropDown " ++ show sel ++ " " ++ show ls ++ ")"

instance eqFormField ∷ (Functor f, Eq (f a), Eq (f Int), Eq (f (L.List a)), Eq (f (L.List Boolean)), Eq (f String), Eq (f HN.HugeNum), Eq (f TB.TimeValueP), Eq (f TB.DateValueP), Eq (f TB.DateTimeValueP), Eq a) ⇒ Eq (FormFieldP f a) where
  eq (TextBox tb1) (TextBox tb2) = tb1 == tb2
  eq (RadioButtons sel1 ls1) (RadioButtons sel2 ls2) = sel1 == sel2 && ls1 == ls2
  eq (CheckBoxes bs1 ls1) (CheckBoxes bs2 ls2) = bs1 == bs2 && ls1 == ls2
  eq (DropDown sel1 ls1) (DropDown sel2 ls2) = sel1 == sel2 && ls1 == ls2
  eq _ _ = false

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

instance arbitraryFormField ∷ (SC.Arbitrary a, Eq a) ⇒ SC.Arbitrary (FormFieldP Expr a) where
  arbitrary = do
    k ← Gen.chooseInt 0.0 3.0
    case k of
      0 → TextBox <<< TB.transTextBox getArbCompose <$> SC.arbitrary
      1 → do
        xse ← genExpr $ distinctListOf SC.arbitrary
        case xse of
          Literal xs → do
            let n = L.length xs
            i ← Gen.chooseInt 0.0 $ Int.toNumber $ n - 1
            pure $ RadioButtons (Literal i) xse
          Unevaluated e → do
            x ← Unevaluated <$> genUnevaluated
            pure $ RadioButtons x xse
      2 → do
        xse ← genExpr $ distinctListOf SC.arbitrary
        case xse of
          Literal xs → do
            bs ← Literal <$> listOfLength (L.length xs) SC.arbitrary
            pure $ CheckBoxes bs xse
          Unevaluated e → do
            bse ← Unevaluated <$> genUnevaluated
            pure $ CheckBoxes bse xse
      _ → do
        xse ← genExpr $ distinctListOf SC.arbitrary
        case xse of
          Literal xs → do
            let n = L.length xs
            mi ← genMaybe $
              Literal <$> Gen.chooseInt 0.0 (Int.toNumber $ n - 1)
            pure $ DropDown mi xse
          Unevaluated e → do
            mx ← genMaybe $ Unevaluated <$> genUnevaluated
            pure $ DropDown mx xse

instance arbitraryFormFieldIdentity ∷ (SC.Arbitrary a, Eq a) ⇒ SC.Arbitrary (FormFieldP Identity a) where
  arbitrary = do
    k ← Gen.chooseInt 0.0 3.0
    case k of
      0 → TextBox <<< TB.transTextBox (\(ArbCompose x) → Compose $ map getArbIdentity x) <$> SC.arbitrary
      1 → do
        xs ← distinctListOf $ getArbIdentity <$> SC.arbitrary
        let n = L.length xs
        i ← Gen.chooseInt 0.0 $ Int.toNumber $ n - 1
        pure $ RadioButtons (Identity i) $ TR.sequence xs
      2 → do
        xs ← map TR.sequence <<< distinctListOf $ getArbIdentity <$> SC.arbitrary
        bs ← listOfLength (L.length $ runIdentity xs) SC.arbitrary
        pure $ CheckBoxes (Identity bs) xs
      _ → do
        xs ← distinctListOf $ getArbIdentity <$> SC.arbitrary
        let n = L.length xs
        mi ← genMaybe $
          pure <$> Gen.chooseInt 0.0 (Int.toNumber $ n - 1)
        pure $ DropDown mi $ TR.sequence xs


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
      CheckBoxes bs xs → \gen → do
        SC.coarbitrary (ArbIdentity $ runIdentity bs) gen
        SC.coarbitrary (ArbIdentity $ runIdentity xs) gen
      DropDown mx xs → \gen → do
        SC.coarbitrary (ArbIdentity <<< runIdentity <$> mx) gen
        SC.coarbitrary (ArbIdentity $ runIdentity xs) gen

listOf
  ∷ ∀ f a
  . (Monad f)
  ⇒ Gen.GenT f a
  → Gen.GenT f (L.List a)
listOf =
  map (L.toList <<< uncurry A.cons)
    <<< Gen.arrayOf1

listOfLength
  ∷ ∀ f a
  . (Monad f)
  ⇒ Int
  → Gen.GenT f a
  → Gen.GenT f (L.List a)
listOfLength i =
  map L.toList
    <<< Gen.vectorOf i

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
  map f (Literal a) = Literal (f a)
  map f (Unevaluated e) = Unevaluated e

instance eqExpr ∷ (Eq a) ⇒ Eq (Expr a) where
  eq (Literal a) (Literal b) = a == b
  eq (Unevaluated e) (Unevaluated f) = e == f
  eq _ _ = false

instance showExpr ∷ (Show a) ⇒ Show (Expr a) where
  show (Literal a) = "(Literal " ++ show a ++ ")"
  show (Unevaluated e) = "(Unevaluated " ++ show e ++ ")"

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
