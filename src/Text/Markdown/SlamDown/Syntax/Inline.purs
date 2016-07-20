module Text.Markdown.SlamDown.Syntax.Inline
  ( Inline(..)
  , LinkTarget(..)
  ) where

import Prelude

import Data.List as L
import Data.Maybe as M

import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen

import Text.Markdown.SlamDown.Syntax.FormField (FormField)

data Inline a
  = Str String
  | Entity String
  | Space
  | SoftBreak
  | LineBreak
  | Emph (L.List (Inline a))
  | Strong (L.List (Inline a))
  | Code Boolean String
  | Link (L.List (Inline a)) LinkTarget
  | Image (L.List (Inline a)) String
  | FormField String Boolean (FormField a)

instance functorInline ∷ Functor Inline where
  map f =
    case _ of
      Str s → Str s
      Entity s → Entity s
      Space → Space
      SoftBreak → SoftBreak
      LineBreak → LineBreak
      Emph is → Emph (map f <$> is)
      Strong is → Strong (map f <$> is)
      Code b s → Code b s
      Link is tgt → Link (map f <$> is) tgt
      Image is tgt → Image (map f <$> is) tgt
      FormField str b ff → FormField str b (f <$> ff)

instance showInline ∷ (Show a) ⇒ Show (Inline a) where
  show (Str s) = "(Str " <> show s <> ")"
  show (Entity s) = "(Entity " <> show s <> ")"
  show Space = "Space"
  show SoftBreak = "SoftBreak"
  show LineBreak = "LineBreak"
  show (Emph is) = "(Emph " <> show is <> ")"
  show (Strong is) = "(Strong " <> show is <> ")"
  show (Code e s) = "(Code " <> show e <> " " <> show s <> ")"
  show (Link is tgt) = "(Link " <> show is <> " " <> show tgt <> ")"
  show (Image is uri) = "(Image " <> show is <> " " <> show uri <> ")"
  show (FormField l r f) = "(FormField " <> show l <> " " <> show r <> " " <> show f <> ")"

derive instance eqInline ∷ (Eq a, Ord a) ⇒ Eq (Inline a)
derive instance ordInline ∷ (Ord a) ⇒ Ord (Inline a)

-- | Nota bene: this does not generate any recursive structure
instance arbitraryInline ∷ (SCA.Arbitrary a, Eq a) ⇒ SCA.Arbitrary (Inline a) where
  arbitrary = do
    k ← Gen.chooseInt 0.0 10.0
    case k of
      0 → Str <$> SCA.arbitrary
      1 → Entity <$> SCA.arbitrary
      2 → pure Space
      3 → pure SoftBreak
      4 → pure LineBreak
      5 → Code <$> SCA.arbitrary <*> SCA.arbitrary
      6 → FormField <$> SCA.arbitrary <*> SCA.arbitrary <*> SCA.arbitrary
      7 → pure (Emph L.Nil)
      8 → pure (Strong L.Nil)
      9 → Link L.Nil <$> SCA.arbitrary
      _ → Image L.Nil <$> SCA.arbitrary


data LinkTarget
  = InlineLink String
  | ReferenceLink (M.Maybe String)

derive instance eqLinkTarget ∷ Eq LinkTarget
derive instance ordLinkTarget ∷ Ord LinkTarget

instance showLinkTarget ∷ Show LinkTarget where
  show (InlineLink uri) = "(InlineLink " <> show uri <> ")"
  show (ReferenceLink tgt) = "(ReferenceLink " <> show tgt <> ")"

instance arbitraryLinkTarget ∷ SCA.Arbitrary LinkTarget where
  arbitrary = do
    b ← SCA.arbitrary
    if b then InlineLink <$> SCA.arbitrary else ReferenceLink <$> SCA.arbitrary
