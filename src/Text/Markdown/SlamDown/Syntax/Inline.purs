module Text.Markdown.SlamDown.Syntax.Inline
  ( Inline(..)
  , LinkTarget(..)
  ) where

import Prelude
import Data.List as L
import Data.Maybe as M

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as SC

import Text.Markdown.SlamDown.Syntax.FormField

data Inline
  = Str String
  | Entity String
  | Space
  | SoftBreak
  | LineBreak
  | Emph (L.List Inline)
  | Strong (L.List Inline)
  | Code Boolean String
  | Link (L.List Inline) LinkTarget
  | Image (L.List Inline) String
  | FormField String Boolean FormField

instance showInline :: Show Inline where
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

instance eqInline :: Eq Inline where
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
instance arbitraryInline :: SC.Arbitrary Inline where
  arbitrary = do
    k <- SC.chooseInt 0.0 10.0
    case k of
      0 -> Str <$> SC.arbitrary
      1 -> Entity <$> SC.arbitrary
      2 -> pure Space
      3 -> pure SoftBreak
      4 -> pure LineBreak
      5 -> Code <$> SC.arbitrary <*> SC.arbitrary
      6 -> FormField <$> SC.arbitrary <*> SC.arbitrary <*> SC.arbitrary
      7 -> pure (Emph L.Nil)
      8 -> pure (Strong L.Nil)
      9 -> Link L.Nil <$> SC.arbitrary
      _ -> Image L.Nil <$> SC.arbitrary


data LinkTarget
  = InlineLink String
  | ReferenceLink (M.Maybe String)

instance showLinkTarget :: Show LinkTarget where
  show (InlineLink uri)    = "(InlineLink " ++ show uri ++ ")"
  show (ReferenceLink tgt) = "(ReferenceLink " ++ show tgt ++ ")"

instance eqLinkTarget :: Eq LinkTarget where
  eq (InlineLink uri1) (InlineLink uri2) = uri1 == uri2
  eq (ReferenceLink tgt1) (ReferenceLink tgt2) = tgt1 == tgt2
  eq _ _ = false

instance arbitraryLinkTarget :: SC.Arbitrary LinkTarget where
  arbitrary = do
    b <- SC.arbitrary
    if b then InlineLink <$> SC.arbitrary else ReferenceLink <$> SC.arbitrary


