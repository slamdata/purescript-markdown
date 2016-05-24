module Text.Markdown.SlamDown.Eval
  ( eval
  , LanguageId
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Const (Const(..))
import Data.Identity (Identity(..), runIdentity)
import Data.Functor.Compose (Compose(..), decompose)
import Data.List as L
import Data.Maybe as M
import Data.String as S
import Data.Traversable as T

import Text.Markdown.SlamDown.Syntax as SD
import Text.Markdown.SlamDown.Traverse (everywhereM)

type LanguageId = String

eval
  ∷ ∀ m a
  . (Monad m, SD.Value a)
  ⇒ { code ∷ M.Maybe LanguageId → String → m a
    , textBox ∷ SD.TextBox (Const String) → m (SD.TextBox Identity)
    , value ∷ String → m a
    , list ∷ String → m (L.List a)
    }
  → SD.SlamDownP a
  → m (SD.SlamDownP a)
eval fs = everywhereM b i
  where

  b ∷ SD.Block a → m (SD.Block a)
  b (SD.CodeBlock (SD.Fenced true info) code) =
    SD.CodeBlock (SD.Fenced false info) <<< pure <<< SD.renderValue
      <$> fs.code (M.Just info) (S.joinWith "\n" (L.fromList code))
  b other = pure $ other

  i ∷ SD.Inline a → m (SD.Inline a)
  i (SD.Code true code) = SD.Code false <<< SD.renderValue <$> fs.code M.Nothing code
  i (SD.FormField l r field) = SD.FormField l r <$> f field
  i other = pure $ other

  f ∷ SD.FormField a → m (SD.FormField a)
  f (SD.TextBox tb) = SD.TextBox <<< M.fromMaybe tb <$> nbeTextBox tb
    where
      -- normalization-by-evaluation proceeds by evaluating an object into a semantic model
      -- (in this case, `Identity`), and then quoting the result back into the syntax.
      nbeTextBox ∷ SD.TextBox (Compose M.Maybe SD.Expr) → m (M.Maybe (SD.TextBox (Compose M.Maybe SD.Expr)))
      nbeTextBox = evalTextBox >>> map (map quoteTextBox)

      evalTextBox ∷ SD.TextBox (Compose M.Maybe SD.Expr) → m (M.Maybe (SD.TextBox Identity))
      evalTextBox tb = T.sequence $ fs.textBox <$> asCode tb <|> pure <$> asLit tb
        where
          asLit = SD.traverseTextBox (decompose >>> (_ >>= SD.getLiteral) >>> map Identity)
          asCode = SD.traverseTextBox (decompose >>> (_ >>= SD.getUnevaluated) >>> map Const)

      quoteTextBox ∷ SD.TextBox Identity → SD.TextBox (Compose M.Maybe SD.Expr)
      quoteTextBox = SD.transTextBox (runIdentity >>> SD.Literal >>> M.Just >>> Compose)

  f (SD.RadioButtons sel opts) = do
    sel' ← evalExpr fs.value sel
    opts' ← evalExpr fs.list opts
    pure $ SD.RadioButtons sel' (mergeSelection (L.singleton <$> sel') opts')

  f (SD.CheckBoxes sel vals) = do
    sel' ← evalExpr fs.list sel
    vals' ← evalExpr fs.list vals
    pure $ SD.CheckBoxes sel' (mergeSelection sel' vals')

  f (SD.DropDown msel opts) = do
    msel' ← T.traverse (evalExpr fs.value) msel
    opts' ← evalExpr fs.list opts
    pure $ SD.DropDown msel' $ M.maybe opts' (flip mergeSelection opts' <<< map L.singleton) msel'

  mergeSelection ∷ SD.Expr (L.List a) → SD.Expr (L.List a) → SD.Expr (L.List a)
  mergeSelection (SD.Literal sel) (SD.Literal xs) = SD.Literal $ L.union sel xs
  mergeSelection _ exs = exs

  evalExpr ∷ ∀ e. (String → m e) → SD.Expr e → m (SD.Expr e)
  evalExpr _ (SD.Literal a) = pure $ SD.Literal a
  evalExpr e (SD.Unevaluated s) = SD.Literal <$> e s

  getValues ∷ ∀ e. SD.Expr (L.List e) → L.List e
  getValues (SD.Literal vs) = vs
  getValues _ = L.Nil
