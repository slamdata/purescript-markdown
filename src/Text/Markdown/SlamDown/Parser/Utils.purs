module Text.Markdown.SlamDown.Parser.Utils
  ( isWhitespace
  , isEmailAddress
  , parens
  , braces
  , squares
  , skipSpaces
  ) where

import Prelude

import Control.Apply ((<*), (*>))

import Data.String (fromChar)
import Data.String.Regex as R

import Text.Parsing.Parser (Parser())
import Text.Parsing.Parser.Combinators (skipMany)
import Text.Parsing.Parser.String (string, satisfy)

isWhitespace ∷ Char → Boolean
isWhitespace = R.test wsRegex <<< fromChar
  where
  wsRegex ∷ R.Regex
  wsRegex = R.regex "^\\s$" flags

isEmailAddress ∷ String → Boolean
isEmailAddress = R.test wsEmail
  where
  wsEmail ∷ R.Regex
  wsEmail = R.regex """^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""" flags

flags ∷ R.RegexFlags
flags =
  { unicode: false
  , sticky: false
  , multiline: false
  , ignoreCase: false
  , global: false
  }

parens ∷ ∀ a. Parser String a → Parser String a
parens p = string "(" *> skipSpaces *> p <* skipSpaces <* string ")"

braces ∷ ∀ a. Parser String a → Parser String a
braces p = string "{" *> skipSpaces *> p <* skipSpaces <* string "}"

squares ∷ ∀ a. Parser String a → Parser String a
squares p = string "[" *> skipSpaces *> p <* skipSpaces <* string "]"

skipSpaces ∷ Parser String Unit
skipSpaces = skipMany (satisfy isWhitespace)
