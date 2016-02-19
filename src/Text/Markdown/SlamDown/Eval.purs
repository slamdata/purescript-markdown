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
  :: forall m a
   . (Monad m, Value a)
  => { code :: String -> m String
     , block :: String -> L.List String -> m String
     , text :: TextBoxType -> String -> m String
     , value :: String -> m a
     , list :: String -> m (L.List a)
     }
  -> SlamDownP a
  -> m (SlamDownP a)
eval fs = everywhereM b i
  where

  b :: Block a -> m (Block a)
  b (CodeBlock (Fenced true info) code) = CodeBlock (Fenced false info) <<< pure <$> fs.block info code
  b other = pure $ other

  i :: Inline a -> m (Inline a)
  i (Code true code) = Code false <$> fs.code code
  i (FormField l r field) = FormField l r <$> f field
  i other = pure $ other

  f :: FormField a -> m (FormField a)
  f (TextBox ty val) = TextBox ty <$> T.traverse (evalExpr (fs.text ty)) val
  f (RadioButtons sel opts) = RadioButtons <$> evalExpr fs.value sel <*> evalExpr fs.list opts
  f (CheckBoxes checkeds vals) = do
    vals' <- evalExpr fs.list vals
    checkeds' <- evalExpr (fs.list >=> \cs -> pure $ map (`F.elem` cs) (getValues vals')) checkeds
    pure $ CheckBoxes checkeds' vals'
  f (DropDown opts default) =
    DropDown <$> evalExpr fs.list opts <*> T.traverse (evalExpr fs.value) default

  evalExpr :: forall e. (String -> m e) -> Expr e -> m (Expr e)
  evalExpr _ (Literal a) = pure $ Literal a
  evalExpr e (Unevaluated s) = Literal <$> e s

  getValues :: forall e. Expr (L.List e) -> L.List e
  getValues (Literal vs) = vs
  getValues _ = L.Nil
