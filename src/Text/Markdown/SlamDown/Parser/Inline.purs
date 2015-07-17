module Text.Markdown.SlamDown.Parser.Inline (parseInlines) where

import Prelude
import Data.Either
import Data.List (List(..), take, many, some, singleton, fromList)
import Data.Foldable (elem)
import Data.Tuple

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Utils

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String (eof, satisfy, string, anyChar, noneOf)

import qualified Data.String as S

import Control.Apply ((<*), (*>))
import Control.Alt ((<|>))
import Control.Lazy (fix)

foreign import error :: forall a. String -> a

parseInlines :: List String -> List Inline
parseInlines s = consolidate $ throwOnError $ runParser (S.joinWith "\n" $ fromList s) inlines
  where
  throwOnError :: forall e a. (Show e) => Either e a -> a
  throwOnError (Left e) = error (show e)
  throwOnError (Right a) = a

consolidate :: List Inline -> List Inline
consolidate Nil = Nil
consolidate (Cons (Str s1) (Cons (Str s2) is)) =
  consolidate (Cons (Str (s1 <> s2)) is)
consolidate (Cons i is) = Cons i $ consolidate is

inlines :: Parser String (List Inline)
inlines = many inline2 <* eof
  where
  inline0 :: Parser String Inline
  inline0 = fix \p -> alphaNumStr
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
  inline2 = try formField
            <|> try inline1
            <|> try image
            <|> other

  alphaNumStr :: Parser String Inline
  alphaNumStr = Str <$> someOf isAlphaNum

  isAlphaNum :: Char -> Boolean
  isAlphaNum c =
    (s >= "a" && s <= "z") ||
    (s >= "A" && s <= "Z") ||
    (s >= "0" && s <= "9")
    where s = S.fromChar c

  emphasis :: Parser String Inline -> (List Inline -> Inline) -> String -> Parser String Inline
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
    f is = Strong $ singleton $ Emph is

  space :: Parser String Inline
  space = (toSpace <<< (S.fromChar <$>)) <$> some (satisfy isWhitespace)
    where
    toSpace cs
      | "\n" `elem` cs =
        case take 2 cs of
          (Cons " " (Cons " " Nil)) -> LineBreak
          _ -> SoftBreak
      | otherwise = Space

  someOf :: (Char -> Boolean) -> Parser String String
  someOf p = (S.fromCharArray <<< fromList) <$> some (satisfy p) 
               
  manyOf :: (Char -> Boolean) -> Parser String String
  manyOf p = (S.fromCharArray <<< fromList) <$> many (satisfy p) 
 

  code :: Parser String Inline
  code = do
    eval <- option false (string "!" *> pure true)
    ticks <- someOf (\x -> S.fromChar x == "`")
    contents <- (S.fromCharArray <<< fromList) <$> manyTill anyChar (string ticks)
    return <<< Code eval <<< trim <<< trimEnd $ contents


  link :: Parser String Inline
  link = Link <$> linkLabel <*> linkTarget
    where
    linkLabel :: Parser String (List Inline)
    linkLabel = string "[" *> manyTill (inline0 <|> other) (string "]")
    
    linkTarget :: Parser String LinkTarget
    linkTarget = inlineLink <|> referenceLink
    
    inlineLink :: Parser String LinkTarget
    inlineLink = InlineLink <<< S.fromCharArray <<< fromList <$> (string "(" *> manyTill anyChar (string ")"))
    
    referenceLink :: Parser String LinkTarget
    referenceLink = ReferenceLink <$> optionMaybe ((S.fromCharArray <<< fromList) <$> (string "[" *> manyTill anyChar (string "]")))
        
  image :: Parser String Inline
  image = Image <$> imageLabel <*> imageUrl
    where
    imageLabel :: Parser String (List Inline)
    imageLabel = string "![" *> manyTill (inline1 <|> other) (string "]")
    
    imageUrl :: Parser String String
    imageUrl = S.fromCharArray <<< fromList <$> (string "(" *> manyTill anyChar (string ")"))
        
  autolink :: Parser String Inline
  autolink = do
    string "<"
    url <- (S.fromCharArray <<< fromList) <$> (anyChar `many1Till` string ">")
    return $ Link (singleton $ Str (autoLabel url)) (InlineLink url)
    where
    autoLabel :: String -> String
    autoLabel s | isEmailAddress s = "mailto:" <> s
                | otherwise = s
     
  entity :: Parser String Inline
  entity = do
    string "&"
    s <- (S.fromCharArray <<< fromList) <$> (noneOf (S.toCharArray ";") `many1Till` string ";")
    return $ Entity $ "&" <> s <> ";"


  formField :: Parser String Inline
  formField = FormField <$> label 
                        <*> (skipSpaces *> required) 
                        <*> (skipSpaces *> string "=" *> skipSpaces *> formElement)
    where
    label = someOf isAlphaNum <|> (S.fromCharArray <<< fromList <$> (string "[" *> manyTill anyChar (string "]")))
    required = option false (string "*" *> pure true)

  formElement :: Parser String FormField
  formElement = try (textBox DateTime dateTime)
            <|> try (textBox Date date)
            <|> try (textBox Time time)
            <|> try (textBox Numeric numeric)
            <|> try (textBox PlainText plainText)
            <|> try radioButtons
            <|> try checkBoxes
            <|> try dropDown
    where
    textBox :: TextBoxType -> Parser String Unit -> Parser String FormField
    textBox ty p = TextBox ty <$>
                   (p *>
                    skipSpaces *>
                    optionMaybe
                    (parens (expr id (manyOf
                                      (\x -> S.fromChar x /= ")")))))

    und :: Parser String Unit
    und = void $ someOf (\x -> S.fromChar x == "_")

    dash :: Parser String Unit
    dash = void $ string "-"

    colon :: Parser String Unit
    colon = void $ string ":"

    hash :: Parser String Unit
    hash = void $ string "#"

    plainText :: Parser String Unit
    plainText = und

    numeric :: Parser String Unit
    numeric = hash *> und

    date :: Parser String Unit
    date = und *> skipSpaces *> dash *> skipSpaces *> und *> skipSpaces *> dash *> skipSpaces *> und

    time :: Parser String Unit
    time = und *> skipSpaces *> colon *> skipSpaces *> und

    dateTime :: Parser String Unit
    dateTime = date *> skipSpaces *> time

    radioButtons :: Parser String FormField
    radioButtons = do
      def <- expr parens $ string "(x)" *> skipSpaces *> someOf isAlphaNum
      skipSpaces
      ls <- expr id $ many (try (skipSpaces *> string "()" *> skipSpaces *> someOf isAlphaNum))
      return $ RadioButtons def ls

    checkBoxes :: Parser String FormField
    checkBoxes = literalCheckBoxes <|> evaluatedCheckBoxes
      where
      literalCheckBoxes = do
        ls <- some $ try do
          skipSpaces
          b <- (string "[x]" *> pure true) <|> (string "[]" *> pure false)
          skipSpaces
          l <- someOf isAlphaNum
          return $ Tuple b l
        return $ CheckBoxes (Literal (map fst ls)) (Literal (map snd ls))

      evaluatedCheckBoxes = CheckBoxes <$> squares evaluated <*> (skipSpaces *> evaluated)

    dropDown :: Parser String FormField
    dropDown = do
      ls <- braces $ expr id $ (try (skipSpaces *> someOf isAlphaNum)) `sepBy` (skipSpaces *> string ",")
      sel <- optionMaybe $ skipSpaces *> (parens $ expr id $ someOf isAlphaNum)
      return $ DropDown ls sel

    expr :: forall a. (forall e. Parser String e -> Parser String e) ->
            Parser String a -> Parser String (Expr a)
    expr f p = try (f evaluated) <|> Literal <$> p

    evaluated :: forall a. Parser String (Expr a)
    evaluated = do
      string "!"
      ticks <- someOf (\x -> S.fromChar x == "`")
      Evaluated <$> (S.fromCharArray <<< fromList) <$> manyTill anyChar (string ticks)

  other :: Parser String Inline
  other = do
    c <- S.fromChar <$> anyChar
    if c == "\\"
       then (Str <<< S.fromChar) <$> anyChar
            <|> (satisfy (\x -> S.fromChar x == "\n") *> pure LineBreak)
            <|> pure (Str "\\")
       else pure (Str c)

