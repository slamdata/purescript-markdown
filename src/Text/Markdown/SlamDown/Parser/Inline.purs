module Text.Markdown.SlamDown.Parser.Inline
  ( parseInlines
  , validateTextOfType
  , validateFormField
  , validateInline
  )
  where

import Prelude

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Control.Lazy (fix)

import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.List (List(..), take, many, some, singleton, fromList, length)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Validation (V(), invalid, runV)
import qualified Data.String as S

import Text.Parsing.Parser (Parser(), ParseError(..), runParser)
import Text.Parsing.Parser.Combinators (try, option, optional, optionMaybe, manyTill, many1Till, sepBy, (<?>))
import Text.Parsing.Parser.String (eof, satisfy, string, anyChar, noneOf)

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Utils

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

someOf :: (Char -> Boolean) -> Parser String String
someOf p = (S.fromCharArray <<< fromList) <$> some (satisfy p)

manyOf :: (Char -> Boolean) -> Parser String String
manyOf p = (S.fromCharArray <<< fromList) <$> many (satisfy p)

isNumeric :: Char -> Boolean
isNumeric c =
  s >= "0" && s <= "9"
  where
    s = S.fromChar c

dash :: Parser String Unit
dash = void $ string "-"

colon :: Parser String Unit
colon = void $ string ":"

dot :: Parser String Unit
dot = void $ string "."

hash :: Parser String Unit
hash = void $ string "#"

type TextParserKit
  = { plainText :: Parser String String
    , numeric :: Parser String String
    , numericPrefix :: Parser String Unit
    }

parseTextOfType :: TextParserKit -> TextBoxType -> Parser String String
parseTextOfType kit tbt = go tbt <?> show tbt
  where
    go PlainText = kit.plainText
    go Numeric = do
      kit.numericPrefix
      m <- kit.numeric
      skipSpaces
      n <- optionMaybe $ dot *> skipSpaces *> kit.numeric
      pure $ m ++ maybe "" (":" ++) n
    go Time = do
      hh <- kit.numeric
      skipSpaces *> colon *> skipSpaces
      mm <- kit.numeric
      pure $ hh ++ ":" ++ mm
    go Date = do
      m <- kit.numeric
      skipSpaces *> dash *> skipSpaces
      d <- kit.numeric
      skipSpaces *> dash *> skipSpaces
      y <- kit.numeric
      pure $ m ++ "-" ++ d ++ "-" ++ y
    go DateTime = do
      date <- go Date
      skipSpaces
      time <- go Time
      pure $ date ++ " " ++ time

validateTextOfType :: TextBoxType -> String -> V (Array String) String
validateTextOfType tbt txt =
  case runParser txt $ parseTextOfType kit tbt of
    Left (ParseError err) -> invalid [err.message]
    Right res -> pure res
  where
    kit =
      { plainText : manyOf \_ -> true
      , numeric : someOf isNumeric
      , numericPrefix : optional hash
      }

validateFormField :: FormField -> V (Array String) FormField
validateFormField field =
  case field of
    TextBox tbt (Just (Literal def)) ->
      runV
        (invalid <<< map ("Invalid text box: " ++))
        (pure <<< TextBox tbt <<< Just <<< Literal)
        (validateTextOfType tbt def)
    CheckBoxes (Literal bs) (Literal ls) | length bs /= length ls ->
      invalid ["Invalid checkboxes"]
    DropDown (Literal ls) (Just (Literal def)) | not (def `elem` ls) ->
      invalid ["Invalid dropdown"]
    _ -> pure field

validateInline :: Inline -> V (Array String) Inline
validateInline inl =
  case inl of
    Emph inls -> Emph <$> traverse validateInline inls
    Strong inls -> Strong <$> traverse validateInline inls
    Link inls targ -> Link <$> traverse validateInline inls <*> pure targ
    Image inls str -> Image <$> traverse validateInline inls <*> pure str
    FormField str b ff -> FormField str b <$> validateFormField ff
    _ -> pure inl


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

  code :: Parser String Inline
  code = do
    eval <- option false (string "!" *> pure true)
    ticks <- someOf (\x -> S.fromChar x == "`")
    contents <- (S.fromCharArray <<< fromList) <$> manyTill anyChar (string ticks)
    return <<< Code eval <<< S.trim $ contents


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
  formElement = try (textBox DateTime)
            <|> try (textBox Date)
            <|> try (textBox Time)
            <|> try (textBox Numeric)
            <|> try (textBox PlainText)
            <|> try radioButtons
            <|> try checkBoxes
            <|> try dropDown
    where

    templateParserKit :: TextParserKit
    templateParserKit =
      { numericPrefix : hash
      , plainText : und
      , numeric : und
      }

    textBox :: TextBoxType -> Parser String FormField
    textBox ty =
      TextBox ty <$>
        (parseTextOfType templateParserKit ty *>
         skipSpaces *>
         optionMaybe
         (parens (expr id (manyOf (\x -> S.fromChar x /= ")")))))

    und :: Parser String String
    und = someOf (\x -> S.fromChar x == "_")

    radioButtons :: Parser String FormField
    radioButtons = do
      let item = someOf \c -> not $ c `elem` ['(',')',' ','!','`']
      def <- expr parens $ string "(x)" *> skipSpaces *> item
      skipSpaces
      ls <- expr id $ many (try (skipSpaces *> string "()" *> skipSpaces *> item))
      return $ RadioButtons def ls

    checkBoxes :: Parser String FormField
    checkBoxes = literalCheckBoxes <|> evaluatedCheckBoxes
      where
      literalCheckBoxes = do
        ls <- some $ try do
          let item = someOf \c -> not $ c `elem` ['[',']',' ','!','`']
          skipSpaces
          b <- (string "[x]" *> pure true) <|> (string "[]" *> pure false)
          skipSpaces
          l <- item
          return $ Tuple b l
        return $ CheckBoxes (Literal (map fst ls)) (Literal (map snd ls))

      evaluatedCheckBoxes = CheckBoxes <$> squares unevaluated <*> (skipSpaces *> unevaluated)

    dropDown :: Parser String FormField
    dropDown = do
      let item = someOf \c -> not $ c `elem` ['{','}',',',' ','!','`','(',')']
      ls <- braces $ expr id $ (try (skipSpaces *> item)) `sepBy` (skipSpaces *> string ",")
      sel <- optionMaybe $ skipSpaces *> (parens $ expr id $ item)
      return $ DropDown ls sel

    expr :: forall a. (forall e. Parser String e -> Parser String e) ->
            Parser String a -> Parser String (Expr a)
    expr f p = try (f unevaluated) <|> Literal <$> p

    unevaluated :: forall a. Parser String (Expr a)
    unevaluated = do
      string "!"
      ticks <- someOf (\x -> S.fromChar x == "`")
      Unevaluated <$> (S.fromCharArray <<< fromList) <$> manyTill anyChar (string ticks)

  other :: Parser String Inline
  other = do
    c <- S.fromChar <$> anyChar
    if c == "\\"
       then (Str <<< S.fromChar) <$> anyChar
            <|> (satisfy (\x -> S.fromChar x == "\n") *> pure LineBreak)
            <|> pure (Str "\\")
       else pure (Str c)

