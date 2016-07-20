module Text.Markdown.SlamDown.Pretty
  ( prettyPrintMd
  , prettyPrintTextBoxValue
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (fold, elem)
import Data.Functor.Compose (Compose, decompose)
import Data.HugeNum as HN
import Data.Identity (Identity(..))
import Data.List as L
import Data.Maybe as M
import Data.Monoid (mempty)
import Data.String as S
import Data.Unfoldable as U

import Text.Markdown.SlamDown.Syntax as SD

unlines ∷ L.List String → String
unlines lst = S.joinWith "\n" $ A.fromFoldable lst

prettyPrintMd ∷ ∀ a. (SD.Value a) ⇒ SD.SlamDownP a → String
prettyPrintMd (SD.SlamDown bs) = unlines $ L.concatMap prettyPrintBlock bs

replicateS ∷ Int → String → String
replicateS n s = fold (const s <$> (1 L... n))

indent ∷ Int → String → String
indent n s = replicateS n " " <> s

overLines ∷ (String → String) → L.List String → L.List String
overLines f = map f <<< L.concatMap lines

lines ∷ String → L.List String
lines "" = mempty
lines s = L.fromFoldable $ S.split "\n" s

prettyPrintBlock ∷ ∀ a. (SD.Value a) ⇒ SD.Block a → L.List String
prettyPrintBlock bl =
  case bl of
    SD.Paragraph is → L.Cons (prettyPrintInlines is) (L.Cons "" L.Nil)
    SD.Header n is → L.singleton (replicateS n "#" <> " " <> prettyPrintInlines is)
    SD.Blockquote bs → overLines ((<>) "> ") (L.concatMap prettyPrintBlock bs)
    SD.Lst lt bss →
      let
        addMarker ∷ L.List String → L.List String
        addMarker L.Nil = L.Nil
        addMarker (L.Cons s ss) =
          let
            m = prettyPrintMarker lt
            len = S.length m
          in
            L.Cons (m <> " " <> s) $ overLines (indent (len + 1)) ss

        prettyPrintMarker ∷ SD.ListType → String
        prettyPrintMarker (SD.Bullet s) = s
        prettyPrintMarker (SD.Ordered s) = "1" <> s

        listItem ∷ L.List (SD.Block a) → L.List String
        listItem = addMarker <<< L.concatMap lines <<< L.concatMap prettyPrintBlock
      in
        L.concatMap listItem bss
    SD.CodeBlock ct ss →
      case ct of
        SD.Indented → indent 4 <$> ss
        SD.Fenced eval info →
          let
            bang
              | eval = "!"
              | otherwise = ""
          in
            L.singleton (bang <> "```" <> info) <> ss <> L.singleton "```"
    SD.LinkReference l url → L.singleton $ squares l <> ": " <> url
    SD.Rule → L.singleton "***"

prettyPrintInlines ∷ ∀ a. (SD.Value a) ⇒ L.List (SD.Inline a) → String
prettyPrintInlines is = S.joinWith "" $ A.fromFoldable $ (map prettyPrintInline is)

prettyPrintInline ∷ ∀ a. (SD.Value a) ⇒ SD.Inline a → String
prettyPrintInline il =
  case il of
    SD.Str s → s
    SD.Entity s → s
    SD.Space → " "
    SD.SoftBreak → "\n"
    SD.LineBreak → "  \n"
    SD.Emph is → "*" <> prettyPrintInlines is <> "*"
    SD.Strong is → "**" <> prettyPrintInlines is <> "**"
    SD.Code e s →
      let
        bang = if e then "!" else ""
      in
        bang <> "`" <> s <> "`"
    SD.Link is tgt → "[" <> prettyPrintInlines is <> "]" <> printTarget tgt
    SD.Image is url → "![" <> prettyPrintInlines is <> "](" <> url <> ")"
    SD.FormField l r e →
      let
        star = if r then "*" else" "
      in
        esc l <> star <> " = " <> prettyPrintFormElement e
      where

      esc s = M.maybe s (const $ "[" <> s <> "]") $ S.indexOf " " s

      printTarget ∷ SD.LinkTarget → String
      printTarget (SD.InlineLink url) = parens url
      printTarget (SD.ReferenceLink tgt) = squares (M.fromMaybe "" tgt)


prettyPrintTextBoxValue ∷ SD.TextBox Identity → String
prettyPrintTextBoxValue t =
  case t of
    SD.PlainText (Identity def) → def
    SD.Numeric (Identity def) →
      let s = HN.toString def in
      M.fromMaybe s $ S.stripSuffix "." $ HN.toString def
    SD.Date (Identity def) → prettyPrintDate def
    SD.Time prec (Identity def) → prettyPrintTime prec def
    SD.DateTime prec (Identity def) → prettyPrintDateTime prec def

prettyPrintDate ∷ SD.DateValue → String
prettyPrintDate { day, month, year } =
  printIntPadded 4 year
    <> "-"
    <> printIntPadded 2 month
    <> "-"
    <> printIntPadded 2 day

prettyPrintTime ∷ SD.TimePrecision → SD.TimeValue → String
prettyPrintTime prec { hours, minutes, seconds }=
  printIntPadded 2 hours
    <> ":"
    <> printIntPadded 2 minutes
    <> case prec of
        SD.Seconds -> ":" <> printIntPadded 2 (M.fromMaybe 0 seconds)
        _ -> ""

prettyPrintDateTime ∷ SD.TimePrecision → SD.DateTimeValue → String
prettyPrintDateTime prec { date, time } =
  prettyPrintDate date
    <> "T"
    <> prettyPrintTime prec time

printIntPadded ∷ Int → Int → String
printIntPadded l i =
  if dl > 0
  then S.fromCharArray (U.replicate dl '0') <> s
  else s
  where
    s = show i
    dl = l - S.length s

prettyPrintTextBox ∷ SD.TextBox (Compose M.Maybe SD.Expr) → String
prettyPrintTextBox t =
  prettyPrintTemplate t
    <> M.maybe "" (\x → " (" <> prettyPrintDefault x <> ")") (SD.traverseTextBox decompose t)
  where
    prettyPrintTemplate ∷ ∀ f. SD.TextBox f → String
    prettyPrintTemplate t =
      case t of
        SD.PlainText _ → "______"
        SD.Numeric _ → "#______"
        SD.Date _ → "__-__-____"
        SD.Time SD.Minutes _ → "__:__"
        SD.Time SD.Seconds _ → "__:__:__"
        SD.DateTime SD.Minutes _ → "__-__-____ __:__"
        SD.DateTime SD.Seconds _ → "__-__-____ __:__:__"

    prettyPrintDefault ∷ SD.TextBox SD.Expr → String
    prettyPrintDefault t =
      case t of
        SD.PlainText def → prettyPrintExpr id id def
        SD.Numeric def → prettyPrintExpr id HN.toString def
        SD.Date def → prettyPrintExpr id prettyPrintDate def
        SD.Time prec def → prettyPrintExpr id (prettyPrintTime prec) def
        SD.DateTime prec def → prettyPrintExpr id (prettyPrintDateTime prec) def


prettyPrintFormElement ∷ ∀ a. (SD.Value a) ⇒ SD.FormField a → String
prettyPrintFormElement el =
  case el of
    SD.TextBox tb → prettyPrintTextBox tb
    SD.RadioButtons (SD.Literal sel) (SD.Literal ls) →
      let
        radioButton l = (if l == sel then "(x) " else "() ") <> SD.renderValue l
      in
        S.joinWith " " $ A.fromFoldable (map radioButton ls)
    SD.RadioButtons (SD.Unevaluated bs) (SD.Unevaluated ls) →
      "(!`" <> bs <> "`) !`" <> ls <> "`"
    SD.CheckBoxes (SD.Literal sel) (SD.Literal ls) →
      let
        checkBox l = (if elem l sel then "[x] " else "[] ") <> SD.renderValue l
      in
        S.joinWith " " <<< A.fromFoldable $ checkBox <$> ls
    SD.CheckBoxes (SD.Unevaluated bs) (SD.Unevaluated ls) →
      "[!`" <> bs <> "`] !`" <> ls <> "`"
    SD.DropDown sel lbls →
      braces (prettyPrintExpr id (A.fromFoldable >>> map SD.renderValue >>> S.joinWith ", ") lbls)
        <> M.maybe "" (parens <<< prettyPrintExpr id SD.renderValue) sel
    _ → "Unsupported form element"

prettyPrintExpr ∷ ∀ a. (String → String) → (a → String) → SD.Expr a → String
prettyPrintExpr _ f (SD.Literal a) = f a
prettyPrintExpr wrap _ (SD.Unevaluated c) = wrap $ "!`" <> c <> "`"

parens ∷ String → String
parens s = "(" <> s <> ")"

braces ∷ String → String
braces s = "{" <> s <> "}"

squares ∷ String → String
squares s = "[" <> s <> "]"
