module Text.Markdown.SlamDown.Parser.Utils where

import Data.Array (reverse)
    
import qualified Data.Char as S
import qualified Data.String as S
import qualified Data.String.Regex as R

isWhitespace :: String -> Boolean
isWhitespace = R.test wsRegex
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
trim = S.dropWhile (isWhitespace <<< S.charString)

trimEnd :: String -> String
trimEnd = reverseString <<< trim <<< reverseString
  where
  reverseString :: String -> String
  reverseString = S.fromCharArray <<< reverse <<< S.toCharArray