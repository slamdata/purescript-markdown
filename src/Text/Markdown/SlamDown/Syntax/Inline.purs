module Text.Markdown.SlamDown.Syntax.Inline
  ( Inline(..)
  , LinkTarget(..)
  ) where

import Prelude
import Data.List as L
import Data.Maybe as M

import Test.StrongCheck as SC
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
  map f x =
    case x of
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
  show (Str s) = "(Str " ++ show s ++ ")"
  show (Entity s) = "(Entity " ++ show s ++ ")"
  show Space = "Space"
  show SoftBreak = "SoftBreak"
  show LineBreak = "LineBreak"
  show (Emph is) = "(Emph " ++ show is ++ ")"
  show (Strong is) = "(Strong " ++ show is ++ ")"
  show (Code e s) = "(Code " ++ show e ++ " " ++ show s ++ ")"
  show (Link is tgt) = "(Link " ++ show is ++ " " ++ show tgt ++ ")"
  show (Image is uri) = "(Image " ++ show is ++ " " ++ show uri ++ ")"
  show (FormField l r f) = "(FormField " ++ show l ++ " " ++ show r ++ " " ++ show f ++ ")"

instance eqInline ∷ (Eq a) ⇒ Eq (Inline a) where
  eq (Str s1) (Str s2) = s1 == s2
  eq (Entity s1) (Entity s2) = s1 == s2
  eq Space Space = true
  eq SoftBreak SoftBreak = true
  eq LineBreak LineBreak = true
  eq (Emph is1) (Emph is2) = is1 == is2
  eq (Strong is1) (Strong is2) = is1 == is2
  eq (Code b1 s1) (Code b2 s2) = b1 == b2 && s1 == s2
  eq (Link is1 tgt1) (Link is2 tgt2) = is1 == is2 && tgt1 == tgt2
  eq (Image is1 s1) (Image is2 s2) = is1 == is2 && s1 == s2
  eq (FormField s1 b1 f1) (FormField s2 b2 f2) = s1 == s2 && b1 == b2 && f1 == f2
  eq _ _ = false

-- | Nota bene: this does not generate any recursive structure
instance arbitraryInline ∷ (SC.Arbitrary a, Eq a) ⇒ SC.Arbitrary (Inline a) where
  arbitrary = do
    k ← Gen.chooseInt 0.0 10.0
    case k of
      0 → Str <$> SC.arbitrary
      1 → Entity <$> SC.arbitrary
      2 → pure Space
      3 → pure SoftBreak
      4 → pure LineBreak
      5 → Code <$> SC.arbitrary <*> SC.arbitrary
      6 → FormField <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      7 → pure (Emph L.Nil)
      8 → pure (Strong L.Nil)
      9 → Link L.Nil <$> SC.arbitrary
      _ → Image L.Nil <$> SC.arbitrary


data LinkTarget
  = InlineLink String
  | ReferenceLink (M.Maybe String)

instance showLinkTarget ∷ Show LinkTarget where
  show (InlineLink uri) = "(InlineLink " ++ show uri ++ ")"
  show (ReferenceLink tgt) = "(ReferenceLink " ++ show tgt ++ ")"

instance eqLinkTarget ∷ Eq LinkTarget where
  eq (InlineLink uri1) (InlineLink uri2) = uri1 == uri2
  eq (ReferenceLink tgt1) (ReferenceLink tgt2) = tgt1 == tgt2
  eq _ _ = false

instance arbitraryLinkTarget ∷ SC.Arbitrary LinkTarget where
  arbitrary = do
    b ← SC.arbitrary
    if b then InlineLink <$> SC.arbitrary else ReferenceLink <$> SC.arbitrary

