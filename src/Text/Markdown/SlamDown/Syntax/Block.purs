module Text.Markdown.SlamDown.Syntax.Block
  ( Block(..)
  , ListType(..)
  , CodeBlockType(..)
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.List as L
import Data.Ord (class Ord1)
import Text.Markdown.SlamDown.Syntax.Inline (Inline)

data Block a
  = Paragraph (L.List (Inline a))
  | Header Int (L.List (Inline a))
  | Blockquote (L.List (Block a))
  | Lst ListType (L.List (L.List (Block a)))
  | CodeBlock CodeBlockType (L.List String)
  | LinkReference String String
  | Rule

derive instance functorBlock ∷ Functor Block

instance showBlock ∷ Show a ⇒ Show (Block a) where
  show (Paragraph is) = "(Paragraph " <> show is <> ")"
  show (Header n is) = "(Header " <> show n <> " " <> show is <> ")"
  show (Blockquote bs) = "(Blockquote " <> show bs <> ")"
  show (Lst lt bss) = "(List " <> show lt <> " " <> show bss <> ")"
  show (CodeBlock ca s) = "(CodeBlock " <> show ca <> " " <> show s <> ")"
  show (LinkReference l uri) = "(LinkReference " <> show l <> " " <> show uri <> ")"
  show Rule = "Rule"

derive instance eqBlock ∷ Eq a ⇒ Eq (Block a)
derive instance eq1Block ∷ Eq1 Block
derive instance ordBlock ∷ Ord a ⇒ Ord (Block a)
derive instance ord1Block ∷ Ord1 Block

data ListType
  = Bullet String
  | Ordered String

instance showListType ∷ Show ListType where
  show (Bullet s) = "(Bullet " <> show s <> ")"
  show (Ordered s) = "(Ordered " <> show s <> ")"

derive instance eqListType ∷ Eq ListType
derive instance ordListType ∷ Ord ListType

data CodeBlockType
  = Indented
  | Fenced Boolean String

instance showCodeBlockType ∷ Show CodeBlockType where
  show Indented = "Indented"
  show (Fenced evaluated info) = "(Fenced " <> show evaluated <> " " <> show info <> ")"

derive instance eqCodeBlockType ∷ Eq CodeBlockType
derive instance ordCodeBlockType ∷ Ord CodeBlockType
