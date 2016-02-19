module Text.Markdown.SlamDown.Eval
  ( eval
  ) where

import Prelude
import Control.Bind ((>=>))
import Data.Foldable as F
import Data.List as L
import Data.Traversable as T

import Text.Markdown.SlamDown.Syntax
import Text.Markdown.SlamDown.Traverse

eval
  :: forall m
   . (Monad m)
  => { code :: String -> m String
     , block :: String -> L.List String -> m String
     , text :: TextBoxType -> String -> m String
     , value :: String -> m String
     , list :: String -> m (L.List String)
     }
  -> SlamDown
  -> m SlamDown
eval fs = everywhereM b i
  where

  b :: Block -> m Block
  b (CodeBlock (Fenced true info) code) = CodeBlock (Fenced false info) <<< pure <$> fs.block info code
  b other = pure $ other

  i :: Inline -> m Inline
  i (Code true code) = Code false <$> fs.code code
  i (FormField l r field) = FormField l r <$> f field
  i other = pure $ other

  f :: FormField -> m FormField
  f (TextBox ty val) = TextBox ty <$> T.traverse (evalExpr (fs.text ty)) val
  f (RadioButtons sel opts) = RadioButtons <$> evalExpr fs.value sel <*> evalExpr fs.list opts
  f (CheckBoxes checkeds vals) = do
    vals' <- evalExpr fs.list vals
    checkeds' <- evalExpr (fs.list >=> \cs -> pure $ map (`F.elem` cs) (getValues vals')) checkeds
    pure $ CheckBoxes checkeds' vals'
  f (DropDown opts default) = DropDown <$> evalExpr fs.list opts <*> T.traverse (evalExpr fs.value) default

  evalExpr :: forall a. (String -> m a) -> Expr a -> m (Expr a)
  evalExpr _ (Literal a) = pure $ Literal a
  evalExpr e (Unevaluated s) = Literal <$> e s

  getValues :: forall a. Expr (L.List a) -> L.List a
  getValues (Literal vs) = vs
  getValues _ = L.Nil
