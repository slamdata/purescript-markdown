module Text.Markdown.SlamDown.Parser.Inline (parseInlines) where
    
import Data.Either 
import Data.Array (take)   
import Data.Foldable (elem)
    
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Utils

import Text.Parsing.Parser 
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String (eof, satisfy, string, char, noneOf)

import qualified Data.String as S

import Control.Apply ((<*), (*>))
import Control.Alt ((<|>))
import Control.Alternative (many, some)
import Control.Lazy (fix1)
    
foreign import error
  "function error(s) {\
  \  throw new Error(s);\
  \}" :: forall a. String -> a
    
parseInlines :: [String] -> [Inline]
parseInlines s = consolidate $ throwOnError $ runParser (S.joinWith "\n" s) inlines
  where
  throwOnError :: forall e a. (Show e) => Either e a -> a
  throwOnError (Left e) = error (show e)
  throwOnError (Right a) = a
  
consolidate :: [Inline] -> [Inline]
consolidate [] = []
consolidate (Str s1 : Str s2 : is) = consolidate (Str (s1 <> s2) : is)
consolidate (i : is) = i : consolidate is
  
inlines :: Parser String [Inline]
inlines = many inline2 <* eof
  where
  inline0 :: Parser String Inline
  inline0 = fix1 \p -> alphaNumStr
     <|> space
     <|> strongEmph p
     <|> strong p
     <|> emph p
     <|> code
     <|> autolink
     <|> entity 
     
  inline1 :: Parser String Inline
  inline1 = try inline0
            <|> try link
           
  inline2 :: Parser String Inline 
  inline2 = try inline1
            <|> try image
            <|> other 
      
  alphaNumStr :: Parser String Inline
  alphaNumStr = Str <$> someOf isAlphaNum
  
  isAlphaNum :: String -> Boolean
  isAlphaNum s =
    (s >= "a" && s <= "z") ||
    (s >= "A" && s <= "Z") ||
    (s >= "0" && s <= "9")
        
  emphasis :: Parser String Inline -> ([Inline] -> Inline) -> String -> Parser String Inline
  emphasis p f s = do
    string s
    f <$> manyTill p (string s)

  emph :: Parser String Inline -> Parser String Inline
  emph p = emphasis p Emph "*" <|> emphasis p Emph "_"

  strong :: Parser String Inline -> Parser String Inline
  strong p = emphasis p Strong "**" <|> emphasis p Strong "__"

  strongEmph :: Parser String Inline -> Parser String Inline
  strongEmph p = emphasis p f "***" <|> emphasis p f "___"
    where
    f is = Strong [Emph is]
        
  space :: Parser String Inline
  space = toSpace <$> some (satisfy isWhitespace)
    where
    toSpace cs | "\n" `elem` cs = case take 2 cs of
                                    [" ", " "] -> LineBreak
                                    _ -> SoftBreak
               | otherwise = Space
               
  someOf :: (String -> Boolean) -> Parser String String
  someOf p = S.joinWith "" <$> some (satisfy p) 
      
  code :: Parser String Inline
  code = do
    ticks <- someOf ((==) "`")
    let end             = string ticks *> notFollowedBy (string "`")
        nonBacktickSpan = someOf ((/=) "`")
        backtickSpan    = someOf ((==) "`")
    contents <- S.joinWith "" <$> manyTill (nonBacktickSpan <|> backtickSpan) end
    return <<< Code <<< trim <<< trimEnd $ contents
        
  link :: Parser String Inline
  link = Link <$> linkLabel <*> linkUrl
    where
    linkLabel :: Parser String [Inline]
    linkLabel = string "[" *> manyTill inline0 (string "]")
    
    linkUrl :: Parser String String
    linkUrl = S.joinWith "" <$> (string "(" *> manyTill char (string ")"))
        
  image :: Parser String Inline
  image = Image <$> imageLabel <*> imageUrl
    where
    imageLabel :: Parser String [Inline]
    imageLabel = string "![" *> manyTill inline1 (string "]")
    
    imageUrl :: Parser String String
    imageUrl = S.joinWith "" <$> (string "(" *> manyTill char (string ")"))
        
  autolink :: Parser String Inline
  autolink = do
    string "<"
    url <- S.joinWith "" <$> (char `many1Till` string ">")
    return $ Link [Str (autoLabel url)] url
    where
    autoLabel :: String -> String
    autoLabel s | isEmailAddress s = "mailto:" <> s
                | otherwise = s
     
  entity :: Parser String Inline
  entity = do
    string "&"
    s <- S.joinWith "" <$> (noneOf [";"] `many1Till` string ";")
    return $ Entity $ "&" <> s <> ";"
        
  other :: Parser String Inline
  other = do
    c <- char
    if c == "\\"
       then Str <$> char
            <|> (satisfy ((==) "\n") *> pure LineBreak)
            <|> pure (Str "\\")
       else pure (Str c)
  
  scanSpaces :: Parser String Unit
  scanSpaces = skipMany (satisfy ((==) " "))