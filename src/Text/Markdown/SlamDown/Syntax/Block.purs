module Text.Markdown.SlamDown.Syntax.Block
  ( Block(..)
  , ListType(..)
  , CodeBlockType(..)
  ) where

import Prelude
import Data.List as L
import Test.StrongCheck as SC
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

instance functorBlock ∷ Functor Block where
  map f x =
    case x of
      Paragraph is → Paragraph (map f <$> is)
      Header n is → Header n (map f <$> is)
      Blockquote bs → Blockquote (map f <$> bs)
      Lst ty bss → Lst ty (map (map f) <$> bss)
      CodeBlock ty ss → CodeBlock ty ss
      LinkReference l uri → LinkReference l uri
      Rule → Rule

instance showBlock ∷ (Show a) ⇒ Show (Block a) where
  show (Paragraph is) = "(Paragraph " ++ show is ++ ")"
  show (Header n is) = "(Header " ++ show n ++ " " ++ show is ++ ")"
  show (Blockquote bs) = "(Blockquote " ++ show bs ++ ")"
  show (Lst lt bss) = "(List " ++ show lt ++ " " ++ show bss ++ ")"
  show (CodeBlock ca s) = "(CodeBlock " ++ show ca ++ " " ++ show s ++ ")"
  show (LinkReference l uri) = "(LinkReference " ++ show l ++ " " ++ show uri ++ ")"
  show Rule = "Rule"

derive instance eqBlock ∷ (Eq a, Ord a) ⇒ Eq (Block a)
derive instance ordBlock ∷ (Ord a) ⇒ Ord (Block a)

-- | Nota bene: this does not generate any recursive structure
instance arbitraryBlock ∷ (SC.Arbitrary a, Eq a) ⇒ SC.Arbitrary (Block a) where
  arbitrary = do
    k ← Gen.chooseInt 0.0 6.0
    case k of
      0 → Paragraph <$> listOf SC.arbitrary
      1 → Header <$> SC.arbitrary <*> listOf SC.arbitrary
      2 → pure $ Blockquote L.Nil
      3 → Lst <$> SC.arbitrary <*> listOf (pure L.Nil)
      4 → CodeBlock <$> SC.arbitrary <*> listOf SC.arbitrary
      5 → LinkReference <$> SC.arbitrary <*> SC.arbitrary
      _ → pure Rule

listOf ∷ ∀ f a. (Monad f) ⇒ Gen.GenT f a → Gen.GenT f (L.List a)
listOf = map L.toList <<< Gen.arrayOf

data ListType
  = Bullet String
  | Ordered String

instance showListType ∷ Show ListType where
  show (Bullet s) = "(Bullet " ++ show s ++ ")"
  show (Ordered s) = "(Ordered " ++ show s ++ ")"

derive instance eqListType ∷ Eq ListType
derive instance ordListType ∷ Ord ListType

instance arbitraryListType ∷ SC.Arbitrary ListType where
  arbitrary = do
    b ← SC.arbitrary
    if b then Bullet <$> SC.arbitrary else Ordered <$> SC.arbitrary

data CodeBlockType
  = Indented
  | Fenced Boolean String

instance showCodeBlockType ∷ Show CodeBlockType where
  show Indented = "Indented"
  show (Fenced evaluated info) = "(Fenced " ++ show evaluated ++ " " ++ show info ++ ")"

derive instance eqCodeBlockType ∷ Eq CodeBlockType
derive instance ordCodeBlockType ∷ Ord CodeBlockType

instance arbitraryCodeBlockType ∷ SC.Arbitrary CodeBlockType where
  arbitrary = do
    b ← SC.arbitrary
    if b then pure Indented else Fenced <$> SC.arbitrary <*> SC.arbitrary
