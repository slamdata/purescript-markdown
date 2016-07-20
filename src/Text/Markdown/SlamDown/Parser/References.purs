module Text.Markdown.SlamDown.Parser.References
  ( parseLinkReference
  ) where

import Prelude

import Data.Array as A
import Data.Either as E
import Data.Maybe as M
import Data.String as S

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Text.Markdown.SlamDown.Parser.Utils as PU
import Text.Markdown.SlamDown.Syntax as SD

parseLinkReference ∷ ∀ a. String → M.Maybe (SD.Block a)
parseLinkReference = E.either (const M.Nothing) M.Just <<< flip P.runParser linkReference

linkReference ∷ ∀ a. P.Parser String (SD.Block a)
linkReference = do
  l ←
    charsToString <$> do
      PS.string "["
      PU.skipSpaces
      PC.manyTill PS.anyChar (PS.string "]")
  PS.string ":"
  PU.skipSpaces
  uri ← charsToString <$> PC.manyTill PS.anyChar PS.eof
  pure $ SD.LinkReference l uri

  where
    charsToString =
      S.trim
        <<< S.fromCharArray
        <<< A.fromFoldable
