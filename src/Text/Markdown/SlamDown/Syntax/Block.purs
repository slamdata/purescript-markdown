module Text.Markdown.SlamDown.Syntax.Block
  ( Block(..)
  , ListType(..)
  , CodeBlockType(..)
  ) where

import Prelude
import Data.List as L
import Test.StrongCheck as SC
import Test.StrongCheck.Gen as SC

import Text.Markdown.SlamDown.Syntax.Inline

data Block a
  = Paragraph (L.List (Inline a))
  | Header Int (L.List (Inline a))
  | Blockquote (L.List (Block a))
  | Lst ListType (L.List (L.List (Block a)))
  | CodeBlock CodeBlockType (L.List String)
  | LinkReference String String
  | Rule

instance functorBlock :: Functor Block where
  map f x =
    case x of
      Paragraph is -> Paragraph (map f <$> is)
      Header n is -> Header n (map f <$> is)
      Blockquote bs -> Blockquote (map f <$> bs)
      Lst ty bss -> Lst ty (map (map f) <$> bss)
      CodeBlock ty ss -> CodeBlock ty ss
      LinkReference l uri -> LinkReference l uri
      Rule -> Rule

instance showBlock :: (Show a) => Show (Block a) where
  show (Paragraph is) = "(Paragraph " ++ show is ++ ")"
  show (Header n is) = "(Header " ++ show n ++ " " ++ show is ++ ")"
  show (Blockquote bs) = "(Blockquote " ++ show bs ++ ")"
  show (Lst lt bss) = "(List " ++ show lt ++ " " ++ show bss ++ ")"
  show (CodeBlock ca s) = "(CodeBlock " ++ show ca ++ " " ++ show s ++ ")"
  show (LinkReference l uri) = "(LinkReference " ++ show l ++ " " ++ show uri ++ ")"
  show Rule = "Rule"

instance eqBlock :: (Eq a) => Eq (Block a) where
  eq (Paragraph is1) (Paragraph is2) = is1 == is2
  eq (Header n1 is1) (Header n2 is2) = n1 == n2 && is1 == is2
  eq (Blockquote bs1) (Blockquote bs2) = bs1 == bs2
  eq (Lst ty1 bss1) (Lst ty2 bss2) = ty1 == ty2 && bss1 == bss2
  eq (CodeBlock ty1 ss1) (CodeBlock ty2 ss2) = ty1 == ty2 && ss1 == ss2
  eq (LinkReference l1 uri1) (LinkReference l2 uri2) = l1 == l2 && uri1 == uri2
  eq Rule Rule = true
  eq _ _ = false

-- | Nota bene: this does not generate any recursive structure
instance arbitraryBlock :: (SC.Arbitrary a) => SC.Arbitrary (Block a) where
  arbitrary = do
    k <- SC.chooseInt 0.0 6.0
    case k of
      0 -> Paragraph <$> listOf SC.arbitrary
      1 -> Header <$> SC.arbitrary <*> listOf SC.arbitrary
      2 -> pure $ Blockquote L.Nil
      3 -> Lst <$> SC.arbitrary <*> listOf (pure L.Nil)
      4 -> CodeBlock <$> SC.arbitrary <*> listOf SC.arbitrary
      5 -> LinkReference <$> SC.arbitrary <*> SC.arbitrary
      _ -> pure Rule

listOf :: forall f a. (Monad f) => SC.GenT f a -> SC.GenT f (L.List a)
listOf = map L.toList <<< SC.arrayOf

data ListType
  = Bullet String
  | Ordered String

instance showListType :: Show ListType where
  show (Bullet s) = "(Bullet " ++ show s ++ ")"
  show (Ordered s) = "(Ordered " ++ show s ++ ")"

instance eqListType :: Eq ListType where
  eq (Bullet s1)  (Bullet s2) = s1 == s2
  eq (Ordered s1) (Ordered s2) = s1 == s2
  eq _ _ = false

instance arbitraryListType :: SC.Arbitrary ListType where
  arbitrary = do
    b <- SC.arbitrary
    if b then Bullet <$> SC.arbitrary else Ordered <$> SC.arbitrary

data CodeBlockType
  = Indented
  | Fenced Boolean String

instance showCodeBlockType :: Show CodeBlockType where
  show Indented = "Indented"
  show (Fenced evaluated info) = "(Fenced " ++ show evaluated ++ " " ++ show info ++ ")"

instance eqCodeBlockType :: Eq CodeBlockType where
  eq Indented Indented = true
  eq (Fenced b1 s1) (Fenced b2 s2) = b1 == b2 && s1 == s2
  eq _ _ = false

instance arbitraryCodeBlockType :: SC.Arbitrary CodeBlockType where
  arbitrary = do
    b <- SC.arbitrary
    if b then pure Indented else Fenced <$> SC.arbitrary <*> SC.arbitrary

