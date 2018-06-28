module Text.Markdown.SlamDown.Syntax.Inline
  ( Inline(..)
  , LinkTarget(..)
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.List as L
import Data.Maybe as M
import Data.Ord (class Ord1)
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

derive instance functorInline ∷ Functor Inline

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

derive instance eqInline ∷ Eq a ⇒ Eq (Inline a)
derive instance eq1Inline ∷ Eq1 Inline
derive instance ordInline ∷ Ord a ⇒ Ord (Inline a)
derive instance ord1Inline ∷ Ord1 Inline

data LinkTarget
  = InlineLink String
  | ReferenceLink (M.Maybe String)

derive instance eqLinkTarget ∷ Eq LinkTarget
derive instance ordLinkTarget ∷ Ord LinkTarget

instance showLinkTarget ∷ Show LinkTarget where
  show (InlineLink uri) = "(InlineLink " <> show uri <> ")"
  show (ReferenceLink tgt) = "(ReferenceLink " <> show tgt <> ")"
