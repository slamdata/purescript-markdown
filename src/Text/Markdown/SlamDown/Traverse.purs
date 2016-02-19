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

everywhereM :: forall m. (Monad m) => (Block -> m Block) -> (Inline -> m Inline) -> SlamDown -> m SlamDown
everywhereM b i (SlamDown bs) = SlamDown <$> T.traverse b' bs
  where
  b' :: Block -> m Block
  b' (Paragraph is) = (Paragraph <$> T.traverse i' is) >>= b
  b' (Header n is) = (Header n <$> T.traverse i' is) >>= b
  b' (Blockquote bs) = (Blockquote <$> T.traverse b' bs) >>= b
  b' (Lst lt bss) = (Lst lt <$> T.traverse (T.traverse b') bss) >>= b
  b' other = b other

  i' :: Inline -> m Inline
  i' (Emph is) = (Emph <$> T.traverse i' is) >>= i
  i' (Strong is) = (Strong <$> T.traverse i' is) >>= i
  i' (Link is uri) = (flip Link uri <$> T.traverse i' is) >>= i
  i' (Image is uri) = (flip Image uri <$> T.traverse i' is) >>= i
  i' other = i other

everywhere :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
everywhere b i = Id.runIdentity <<< everywhereM (pure <<< b) (pure <<< i)

everywhereTopDownM :: forall m. (Monad m) => (Block -> m Block) -> (Inline -> m Inline) -> SlamDown -> m SlamDown
everywhereTopDownM b i (SlamDown bs) = SlamDown <$> T.traverse (b' <=< b) bs
  where
  b' :: Block -> m Block
  b' (Paragraph is) = Paragraph <$> T.traverse (i' <=< i) is
  b' (Header n is) = Header n <$> T.traverse (i' <=< i) is
  b' (Blockquote bs) = Blockquote <$> T.traverse (b' <=< b) bs
  b' (Lst ty bss) = Lst ty <$> T.traverse (T.traverse (b' <=< b)) bss
  b' other = b other

  i' :: Inline -> m Inline
  i' (Emph is) = Emph <$> T.traverse (i' <=< i) is
  i' (Strong is) = Strong <$> T.traverse (i' <=< i) is
  i' (Link is uri) = flip Link uri <$> T.traverse (i' <=< i) is
  i' (Image is uri) = flip Image uri <$> T.traverse (i' <=< i) is
  i' other = i other

everywhereTopDown :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
everywhereTopDown b i = Id.runIdentity <<< everywhereTopDownM (pure <<< b) (pure <<< i)

everythingM :: forall m r. (Monad m, Monoid r) => (Block -> m r) -> (Inline -> m r) -> SlamDown -> m r
everythingM b i (SlamDown bs) = F.mconcat <$> T.traverse b' bs
  where
  b' :: Block -> m r
  b' x@(Paragraph is) = b x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  b' x@(Header _ is) = b x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  b' x@(Blockquote bs) = b x >>= \r -> F.foldl (<>) r <$> T.traverse b' bs
  b' x@(Lst _ bss) = b x >>= \r -> F.foldl (<>) r <<< join <$> T.traverse (\bs -> T.traverse b' bs) bss
  b' x = b x

  i' :: Inline -> m r
  i' x@(Emph is) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x@(Strong is) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x@(Link is _) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x@(Image is _) = i x >>= \r -> F.foldl (<>) r <$> T.traverse i' is
  i' x = i x

everything :: forall r. (Monoid r) => (Block -> r) -> (Inline -> r) -> SlamDown -> r
everything b i = Id.runIdentity <<< everythingM (pure <<< b) (pure <<< i)
