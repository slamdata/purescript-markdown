module Text.Markdown.SlamDown.Parser.Inline (parseInlines) where

import Data.Either
import Data.Array (map, take)
import Data.Foldable (elem)
import Data.Tuple

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
  inline2 = try formField
            <|> try inline1
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

  manyOf :: (String -> Boolean) -> Parser String String
  manyOf p = S.joinWith "" <$> many (satisfy p)

  code :: Parser String Inline
  code = do
    eval <- option false (string "!" *> pure true)
    ticks <- someOf ((==) "`")
    contents <- S.joinWith "" <$> manyTill char (string ticks)
    return <<< Code eval <<< trim <<< trimEnd $ contents

  link :: Parser String Inline
  link = Link <$> linkLabel <*> linkTarget
    where
    linkLabel :: Parser String [Inline]
    linkLabel = string "[" *> manyTill (inline0 <|> other) (string "]")

    linkTarget :: Parser String LinkTarget
    linkTarget = inlineLink <|> referenceLink

    inlineLink :: Parser String LinkTarget
    inlineLink = InlineLink <<< S.joinWith "" <$> (string "(" *> manyTill char (string ")"))

    referenceLink :: Parser String LinkTarget
    referenceLink = ReferenceLink <$> optionMaybe (S.joinWith "" <$> (string "[" *> manyTill char (string "]")))

  image :: Parser String Inline
  image = Image <$> imageLabel <*> imageUrl
    where
    imageLabel :: Parser String [Inline]
    imageLabel = string "![" *> manyTill (inline1 <|> other) (string "]")

    imageUrl :: Parser String String
    imageUrl = S.joinWith "" <$> (string "(" *> manyTill char (string ")"))

  autolink :: Parser String Inline
  autolink = do
    string "<"
    url <- S.joinWith "" <$> (char `many1Till` string ">")
    return $ Link [Str (autoLabel url)] (InlineLink url)
    where
    autoLabel :: String -> String
    autoLabel s | isEmailAddress s = "mailto:" <> s
                | otherwise = s

  entity :: Parser String Inline
  entity = do
    string "&"
    s <- S.joinWith "" <$> (noneOf [";"] `many1Till` string ";")
    return $ Entity $ "&" <> s <> ";"

  formField :: Parser String Inline
  formField = FormField <$> label
                        <*> (skipSpaces *> required)
                        <*> (skipSpaces *> string "=" *> skipSpaces *> formElement)
    where
    label = someOf isAlphaNum <|> (S.joinWith "" <$> (string "[" *> manyTill char (string "]")))
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
    textBox ty p = TextBox ty <$> (p *> skipSpaces *> optionMaybe (parens (expr id (manyOf ((/=) ")")))))

    und :: Parser String Unit
    und = void $ someOf ((==) "_")

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
      ticks <- someOf ((==) "`")
      Evaluated <$> S.joinWith "" <$> manyTill char (string ticks)

  other :: Parser String Inline
  other = do
    c <- char
    if c == "\\"
       then Str <$> char
            <|> (satisfy ((==) "\n") *> pure LineBreak)
            <|> pure (Str "\\")
       else pure (Str c)

