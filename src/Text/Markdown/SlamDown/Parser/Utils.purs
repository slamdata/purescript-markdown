module Text.Markdown.SlamDown.Parser.Utils where

import Prelude
import Data.Array (reverse)
    
import qualified Data.Char as S
import qualified Data.String as S
import qualified Data.String.Regex as R

import Control.Apply ((<*), (*>))

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators (skipMany)
import Text.Parsing.Parser.String (string, satisfy)

import Text.Markdown.SlamDown

isWhitespace :: Char -> Boolean
isWhitespace = R.test wsRegex <<< S.fromChar
  where
  wsRegex :: R.Regex
  wsRegex = R.regex "^\\s$" flags

isEmailAddress :: String -> Boolean
isEmailAddress = R.test wsEmail
  where
  wsEmail :: R.Regex
  wsEmail = R.regex """^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""" flags

flags :: R.RegexFlags
flags = { unicode: false
        , sticky: false
        , multiline: false
        , ignoreCase: false
        , global: false 
        }
    
trim :: String -> String
trim = S.dropWhile isWhitespace

trimEnd :: String -> String
trimEnd = reverseString <<< trim <<< reverseString
  where
  reverseString :: String -> String
  reverseString = S.fromCharArray <<< reverse <<< S.toCharArray
     
parens :: forall a. Parser String a -> Parser String a
parens p = string "(" *> skipSpaces *> p <* skipSpaces <* string ")"

braces :: forall a. Parser String a -> Parser String a
braces p = string "{" *> skipSpaces *> p <* skipSpaces <* string "}"

squares :: forall a. Parser String a -> Parser String a
squares p = string "[" *> skipSpaces *> p <* skipSpaces <* string "]"

skipSpaces :: Parser String Unit
skipSpaces = skipMany (satisfy (\x -> S.fromChar x == " "))
