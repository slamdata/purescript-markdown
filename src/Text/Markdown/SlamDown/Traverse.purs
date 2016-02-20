module Text.Markdown.SlamDown.Traverse
  ( everywhereM
  , everywhere
  , everywhereTopDownM
  , everywhereTopDown
  , everythingM
  , everything
  ) where

import Prelude
import Control.Bind ((<=<), join)
import Data.Monoid
import Data.Identity as Id
import Data.Foldable as F
import Data.Traversable as T
import Text.Markdown.SlamDown.Syntax

everywhereM
  :: forall m a
   . (Monad m)
  => (Block a -> m (Block a))
  -> (Inline a -> m (Inline a))
  -> SlamDownP a
  -> m (SlamDownP a)
everywhereM b i (SlamDown bs) =
  SlamDown <$> T.traverse b' bs

  where
  b' :: Block a -> m (Block a)
  b' (Paragraph is) = (Paragraph <$> T.traverse i' is) >>= b
  b' (Header n is) = (Header n <$> T.traverse i' is) >>= b
  b' (Blockquote bs) = (Blockquote <$> T.traverse b' bs) >>= b
  b' (Lst lt bss) = (Lst lt <$> T.traverse (T.traverse b') bss) >>= b
  b' other = b other

  i' :: Inline a -> m (Inline a)
  i' (Emph is) = (Emph <$> T.traverse i' is) >>= i
  i' (Strong is) = (Strong <$> T.traverse i' is) >>= i
  i' (Link is uri) = (flip Link uri <$> T.traverse i' is) >>= i
  i' (Image is uri) = (flip Image uri <$> T.traverse i' is) >>= i
  i' other = i other

everywhere
  :: forall a
   . (Block a -> Block a)
  -> (Inline a -> Inline a)
  -> SlamDownP a
  -> SlamDownP a
everywhere b i =
  Id.runIdentity
    <<< everywhereM (pure <<< b) (pure <<< i)

everywhereTopDownM
  :: forall m a
   . (Monad m)
  => (Block a -> m (Block a))
  -> (Inline a -> m (Inline a))
  -> SlamDownP a
  -> m (SlamDownP a)
everywhereTopDownM b i (SlamDown bs) =
  SlamDown <$>
    T.traverse (b' <=< b) bs
  where
  b' :: Block a -> m (Block a)
  b' (Paragraph is) = Paragraph <$> T.traverse (i' <=< i) is
  b' (Header n is) = Header n <$> T.traverse (i' <=< i) is
  b' (Blockquote bs) = Blockquote <$> T.traverse (b' <=< b) bs
  b' (Lst ty bss) = Lst ty <$> T.traverse (T.traverse (b' <=< b)) bss
  b' other = b other

  i' :: Inline a -> m (Inline a)
  i' (Emph is) = Emph <$> T.traverse (i' <=< i) is
  i' (Strong is) = Strong <$> T.traverse (i' <=< i) is
  i' (Link is uri) = flip Link uri <$> T.traverse (i' <=< i) is
  i' (Image is uri) = flip Image uri <$> T.traverse (i' <=< i) is
  i' other = i other

everywhereTopDown
  :: forall a
   . (Block a -> Block a)
  -> (Inline a -> Inline a)
  -> SlamDownP a
  -> SlamDownP a
everywhereTopDown b i =
  Id.runIdentity <<<
    everywhereTopDownM
      (pure <<< b)
      (pure <<< i)

everythingM
  :: forall m a r
   . (Monad m, Monoid r)
  => (Block a -> m r)
  -> (Inline a -> m r)
  -> SlamDownP a
  -> m r
everythingM b i (SlamDown bs) =
  F.mconcat <$> T.traverse b' bs
  where
  b' :: Block a -> m r
  b' x@(Paragraph is) = b x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  b' x@(Header _ is) = b x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  b' x@(Blockquote bs) = b x >>= \r -> F.foldl (<>) r <$> T.traverse b' bs
  b' x@(Lst _ bss) = b x >>= \r -> F.foldl (<>) r <<< join <$> T.traverse (\bs -> T.traverse b' bs) bss
  b' x = b x

  i' :: Inline a -> m r
  i' x@(Emph is) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x@(Strong is) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x@(Link is _) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x@(Image is _) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x = i x

everything
  :: forall r a
   . (Monoid r)
  => (Block a -> r)
  -> (Inline a -> r)
  -> SlamDownP a
  -> r
everything b i =
  Id.runIdentity <<<
    everythingM
      (pure <<< b)
      (pure <<< i)
