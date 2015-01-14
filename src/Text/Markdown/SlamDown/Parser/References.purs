module Text.Markdown.SlamDown.Parser.References (parseLinkReference) where
    
import Data.Maybe
import Data.Either    
    
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Utils

import Text.Parsing.Parser 
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String (eof, satisfy, string, char, noneOf)

import qualified Data.String as S

import Control.Apply ((<*), (*>))
import Control.Alt ((<|>))
import Control.Alternative (many, some)
    
parseLinkReference :: String -> Maybe Block
parseLinkReference s = either (const Nothing) Just $ runParser s linkReference
  
linkReference :: Parser String Block
linkReference = do
  l <- trimEnd <<< S.joinWith "" <$> (string "[" *> skipSpaces *> manyTill char (string "]"))
  string ":"
  skipSpaces
  uri <- trimEnd <<< S.joinWith "" <$> manyTill char eof
  return $ LinkReference l uri