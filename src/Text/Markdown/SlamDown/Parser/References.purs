module Text.Markdown.SlamDown.Parser.References (parseLinkReference) where

import Prelude

import Control.Apply ((*>))

import Data.Either (either)
import Data.List (fromList)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, trim)

import Text.Parsing.Parser (Parser(), runParser)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (eof, string, anyChar)

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Utils

parseLinkReference :: String -> Maybe Block
parseLinkReference s = either (const Nothing) Just $ runParser s linkReference

linkReference :: Parser String Block
linkReference = do
  l <- trim <<< (fromCharArray <<< fromList) <$> (string "[" *> skipSpaces *> manyTill anyChar (string "]"))
  string ":"
  skipSpaces
  uri <- trim <<< (fromCharArray <<< fromList) <$> manyTill anyChar eof
  return $ LinkReference l uri
