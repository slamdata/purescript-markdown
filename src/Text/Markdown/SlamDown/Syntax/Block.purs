module Text.Markdown.SlamDown.Syntax.Block
  ( Block(..)
  , ListType(..)
  , CodeBlockType(..)
  ) where

import Prelude

import Data.Eq (class Eq1)
import Data.List as L
import Data.Ord (class Ord1)
import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen
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

-- | Nota bene: this does not generate any recursive structure
instance arbitraryBlock ∷ (SCA.Arbitrary a, Eq a) ⇒ SCA.Arbitrary (Block a) where
  arbitrary = do
    k ← Gen.chooseInt 0 6
    case k of
      0 → Paragraph <$> listOf SCA.arbitrary
      1 → Header <$> SCA.arbitrary <*> listOf SCA.arbitrary
      2 → pure $ Blockquote L.Nil
      3 → Lst <$> SCA.arbitrary <*> listOf (pure L.Nil)
      4 → CodeBlock <$> SCA.arbitrary <*> listOf SCA.arbitrary
      5 → LinkReference <$> SCA.arbitrary <*> SCA.arbitrary
      _ → pure Rule

listOf ∷ ∀ f a. (Monad f) ⇒ Gen.GenT f a → Gen.GenT f (L.List a)
listOf = map L.fromFoldable <<< Gen.arrayOf

data ListType
  = Bullet String
  | Ordered String

instance showListType ∷ Show ListType where
  show (Bullet s) = "(Bullet " <> show s <> ")"
  show (Ordered s) = "(Ordered " <> show s <> ")"

derive instance eqListType ∷ Eq ListType
derive instance ordListType ∷ Ord ListType

instance arbitraryListType ∷ SCA.Arbitrary ListType where
  arbitrary = do
    b ← SCA.arbitrary
    if b then Bullet <$> SCA.arbitrary else Ordered <$> SCA.arbitrary

data CodeBlockType
  = Indented
  | Fenced Boolean String

instance showCodeBlockType ∷ Show CodeBlockType where
  show Indented = "Indented"
  show (Fenced evaluated info) = "(Fenced " <> show evaluated <> " " <> show info <> ")"

derive instance eqCodeBlockType ∷ Eq CodeBlockType
derive instance ordCodeBlockType ∷ Ord CodeBlockType

instance arbitraryCodeBlockType ∷ SCA.Arbitrary CodeBlockType where
  arbitrary = do
    b ← SCA.arbitrary
    if b then pure Indented else Fenced <$> SCA.arbitrary <*> SCA.arbitrary
