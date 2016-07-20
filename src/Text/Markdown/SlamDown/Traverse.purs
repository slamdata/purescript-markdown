module Text.Markdown.SlamDown.Traverse
  ( everywhereM
  , everywhere
  , everywhereTopDownM
  , everywhereTopDown
  , everythingM
  , everything
  ) where

import Prelude

import Data.Foldable as F
import Data.Identity as Id
import Data.Monoid (class Monoid)
import Data.Traversable as T

import Text.Markdown.SlamDown.Syntax as SD

everywhereM
  ∷ ∀ m a
  . (Monad m)
  ⇒ (SD.Block a → m (SD.Block a))
  → (SD.Inline a → m (SD.Inline a))
  → SD.SlamDownP a
  → m (SD.SlamDownP a)
everywhereM b i (SD.SlamDown bs) =
  SD.SlamDown <$> T.traverse b' bs

  where
  b' ∷ SD.Block a → m (SD.Block a)
  b' (SD.Paragraph is) = (SD.Paragraph <$> T.traverse i' is) >>= b
  b' (SD.Header n is) = (SD.Header n <$> T.traverse i' is) >>= b
  b' (SD.Blockquote bs) = (SD.Blockquote <$> T.traverse b' bs) >>= b
  b' (SD.Lst lt bss) = (SD.Lst lt <$> T.traverse (T.traverse b') bss) >>= b
  b' other = b other

  i' ∷ SD.Inline a → m (SD.Inline a)
  i' (SD.Emph is) = (SD.Emph <$> T.traverse i' is) >>= i
  i' (SD.Strong is) = (SD.Strong <$> T.traverse i' is) >>= i
  i' (SD.Link is uri) = (flip SD.Link uri <$> T.traverse i' is) >>= i
  i' (SD.Image is uri) = (flip SD.Image uri <$> T.traverse i' is) >>= i
  i' other = i other

everywhere
  ∷ ∀ a
  . (SD.Block a → SD.Block a)
  → (SD.Inline a → SD.Inline a)
  → SD.SlamDownP a
  → SD.SlamDownP a
everywhere b i =
  Id.runIdentity
    <<< everywhereM (pure <<< b) (pure <<< i)

everywhereTopDownM
  ∷ ∀ m a
  . (Monad m)
  ⇒ (SD.Block a → m (SD.Block a))
  → (SD.Inline a → m (SD.Inline a))
  → SD.SlamDownP a
  → m (SD.SlamDownP a)
everywhereTopDownM b i (SD.SlamDown bs) =
  SD.SlamDown <$>
    T.traverse (b' <=< b) bs
  where
  b' ∷ SD.Block a → m (SD.Block a)
  b' (SD.Paragraph is) = SD.Paragraph <$> T.traverse (i' <=< i) is
  b' (SD.Header n is) = SD.Header n <$> T.traverse (i' <=< i) is
  b' (SD.Blockquote bs) = SD.Blockquote <$> T.traverse (b' <=< b) bs
  b' (SD.Lst ty bss) = SD.Lst ty <$> T.traverse (T.traverse (b' <=< b)) bss
  b' other = b other

  i' ∷ SD.Inline a → m (SD.Inline a)
  i' (SD.Emph is) = SD.Emph <$> T.traverse (i' <=< i) is
  i' (SD.Strong is) = SD.Strong <$> T.traverse (i' <=< i) is
  i' (SD.Link is uri) = flip SD.Link uri <$> T.traverse (i' <=< i) is
  i' (SD.Image is uri) = flip SD.Image uri <$> T.traverse (i' <=< i) is
  i' other = i other

everywhereTopDown
  ∷ ∀ a
  . (SD.Block a → SD.Block a)
  → (SD.Inline a → SD.Inline a)
  → SD.SlamDownP a
  → SD.SlamDownP a
everywhereTopDown b i =
  Id.runIdentity <<<
    everywhereTopDownM
      (pure <<< b)
      (pure <<< i)

everythingM
  ∷ ∀ m a r
  . (Monad m, Monoid r)
  ⇒ (SD.Block a → m r)
  → (SD.Inline a → m r)
  → SD.SlamDownP a
  → m r
everythingM b i (SD.SlamDown bs) =
  F.fold <$> T.traverse b' bs
  where
  b' ∷ SD.Block a → m r
  b' x@(SD.Paragraph is) = b x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  b' x@(SD.Header _ is) = b x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  b' x@(SD.Blockquote bs) = b x >>= \r → F.foldl (<>) r <$> T.traverse b' bs
  b' x@(SD.Lst _ bss) = b x >>= \r → F.foldl (<>) r <<< join <$> T.traverse (\bs → T.traverse b' bs) bss
  b' x = b x

  i' ∷ SD.Inline a → m r
  i' x@(SD.Emph is) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x@(SD.Strong is) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x@(SD.Link is _) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x@(SD.Image is _) = i x >>= \r → F.foldl (<>) r <$> T.traverse i' is
  i' x = i x

everything
  ∷ ∀ r a
  . (Monoid r)
  ⇒ (SD.Block a → r)
  → (SD.Inline a → r)
  → SD.SlamDownP a
  → r
everything b i =
  Id.runIdentity <<<
    everythingM
      (pure <<< b)
      (pure <<< i)
