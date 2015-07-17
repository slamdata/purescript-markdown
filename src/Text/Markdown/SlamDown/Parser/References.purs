module Text.Markdown.SlamDown.Parser.References (parseLinkReference) where

import Prelude
import Data.Maybe
import Data.Either    
import Data.List (fromList)

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Utils

import Text.Parsing.Parser 
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String (eof, satisfy, string, anyChar, noneOf)


import qualified Data.String as S

import Control.Apply ((<*), (*>))
import Control.Alt ((<|>))
    
parseLinkReference :: String -> Maybe Block
parseLinkReference s = either (const Nothing) Just $ runParser s linkReference
  
linkReference :: Parser String Block
linkReference = do
  l <- trimEnd <<< (S.fromCharArray <<< fromList) <$> (string "[" *> skipSpaces *> manyTill anyChar (string "]"))
  string ":"
  skipSpaces
  uri <- trimEnd <<< (S.fromCharArray <<< fromList) <$> manyTill anyChar eof
  return $ LinkReference l uri
