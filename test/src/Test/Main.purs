module Test.Main where

import Prelude

import Data.DateTime as DT
import Data.Either (Either(..), isLeft)
import Data.Enum (toEnum)
import Data.HugeNum as HN
import Data.Identity as ID
import Data.List as L
import Data.Maybe as M
import Data.Newtype (un)
import Effect (Effect)
import Effect.Console as C
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert, assertEqual)
import Text.Markdown.SlamDown.Eval as SDE
import Text.Markdown.SlamDown.Parser as SDP
import Text.Markdown.SlamDown.Pretty as SDPR
import Text.Markdown.SlamDown.Syntax as SD

testDocument ∷ Either String (SD.SlamDownP String) → Effect Unit
testDocument sd = do
  let printed = SDPR.prettyPrintMd <$> sd
      parsed = printed >>= SDP.parseMd

  C.log
    $ "Original: \n   "
    <> show sd
    <> "\nPrinted:\n   "
    <> show printed
    <> "\nParsed:\n   "
    <> show parsed
  assertEqual { expected: parsed, actual: sd }

failDocument ∷ Either String (SD.SlamDownP String) → Effect Unit
failDocument sd = assert (isLeft sd)

main ∷ Effect Unit
main = do
  testDocument $ SDP.parseMd "Paragraph"
  testDocument $ SDP.parseMd "Paragraph with spaces"
  testDocument $ SDP.parseMd "Paragraph with an entity: &copy;"
  testDocument $ SDP.parseMd "Paragraph with a [link](http://purescript.org)"
  testDocument $ SDP.parseMd "Paragraph with an ![image](image.png)"
  testDocument $ SDP.parseMd "Paragraph with some `embedded code`"
  testDocument $ SDP.parseMd "Paragraph with some !`code which can be evaluated`"
  testDocument $ SDP.parseMd "Paragraph with _emphasis_"
  testDocument $ SDP.parseMd "Paragraph with _emphasis_ and __strong text__"

  testDocument $
    SDP.parseMd
      "Paragraph with a\n\
      \soft break"

  testDocument $
    SDP.parseMd
      "Paragraph with a  \n\
      \line break"

  testDocument $
    SDP.parseMd
      "Two\n\
      \\n\
      \paragraphs"

  testDocument $
    SDP.parseMd
      "Header\n\
      \==="

  testDocument $
    SDP.parseMd
      "# Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    SDP.parseMd
      "## Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    SDP.parseMd
      "### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    SDP.parseMd
      "#### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    SDP.parseMd
      "##### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    SDP.parseMd
      "###### Header\n\
      \\n\
      \Paragraph text"

  testDocument $
    SDP.parseMd
      "Rule:\n\
      \\n\
      \-----"

  testDocument $
    SDP.parseMd
      "A blockquote:\n\
      \\n\
      \> Here is some text\n\
      \> inside a blockquote"

  testDocument $
    SDP.parseMd
      "A nested blockquote:\n\
      \\n\
      \> Here is some text\n\
      \> > Here is some more text"

  testDocument $
    SDP.parseMd
      "An unordered list:\n\
      \\n\
      \* Item 1\n\
      \* Item 2"

  testDocument $
    SDP.parseMd
      "An ordered list:\n\
      \\n\
      \1. Item 1\n\
      \1. Item 2"

  testDocument $
    SDP.parseMd
      "A nested list:\n\
      \\n\
      \1. Item 1\n\
      \1. 1. Item 2\n\
      \   1. Item 3"

  testDocument $
    SDP.parseMd
      "Some indented code:\n\
      \\n\
      \    import Debug.Log\n\
      \    \n\
      \    main = log \"Hello World\""

  testDocument $
    SDP.parseMd
      "Some fenced code:\n\
      \\n\
      \```purescript\n\
      \import Debug.Log\n\
      \\n\
      \main = log \"Hello World\"\n\
      \```"

  testDocument $
    SDP.parseMd
      "Some fenced code which can be evaluated:\n\
      \\n\
      \!~~~purescript\n\
      \import Debug.Log\n\
      \\n\
      \main = log \"Hello World\"\n\
      \~~~"

  let
    probablyParsedCodeForEvaluation =
      SDP.parseMd
        "Some evaluated fenced code:\n\
        \\n\
        \!~~~purescript\n\
        \import Debug.Log\n\
        \\n\
        \main = log \"Hello World\"\n\
        \~~~"

  testDocument
    case probablyParsedCodeForEvaluation of
      Right sd →
        Right
          $ un ID.Identity
          $ SDE.eval
            { code: \_ _ → pure $ SD.stringValue "Evaluated code block!"
            , textBox: \_ t →
                case t of
                  SD.PlainText _ → pure $ SD.PlainText $ pure "Evaluated plain text!"
                  SD.Numeric _ → pure $ SD.Numeric $ pure $ HN.fromNumber 42.0
                  SD.Date _ → pure $ SD.Date $ pure $ unsafeDate 1992 7 30
                  SD.Time (prec@SD.Minutes) _ → pure $ SD.Time prec $ pure $ unsafeTime 4 52 0
                  SD.Time (prec@SD.Seconds) _ → pure $ SD.Time prec $ pure $ unsafeTime 4 52 10
                  SD.DateTime (prec@SD.Minutes) _ →
                    pure $ SD.DateTime prec $ pure $
                      DT.DateTime (unsafeDate 1992 7 30) (unsafeTime 4 52 0)
                  SD.DateTime (prec@SD.Seconds) _ →
                    pure $ SD.DateTime prec $ pure $
                      DT.DateTime (unsafeDate 1992 7 30) (unsafeTime 4 52 10)
            , value: \_ _ → pure $ SD.stringValue "Evaluated value!"
            , list: \_ _ → pure $ L.singleton $ SD.stringValue "Evaluated list!"
            }  sd
      a → a

  testDocument $ SDP.parseMd "name = __ (Phil Freeman)"
  testDocument $ SDP.parseMd "name = __ (!`name`)"
  testDocument $ SDP.parseMd "sex* = (x) male () female () other"
  testDocument $ SDP.parseMd "sex* = (!`def`) !`others`"
  testDocument $ SDP.parseMd "city = {BOS, SFO, NYC} (NYC)"
  testDocument $ SDP.parseMd "city = {!`...`} (!`...`)"
  testDocument $ SDP.parseMd "phones = [] Android [x] iPhone [x] Blackberry"
  testDocument $ SDP.parseMd "phones = [!`...`] !`...`"
  testDocument $ SDP.parseMd "start = __ - __ - ____ (06-06-2015)"
  testDocument $ SDP.parseMd "start = __ - __ - ____ (!`...`)"
  testDocument $ SDP.parseMd "start = __ : __ (10:32 PM)"
  failDocument $ SDP.parseMd "start = __ : __ (10:32:46 PM)"
  failDocument $ SDP.parseMd "start = __ : __ : __ (10:32 PM)"
  testDocument $ SDP.parseMd "start = __ : __ : __ (10:32:46 PM)"
  testDocument $ SDP.parseMd "start = __ : __ (!`...`)"
  testDocument $ SDP.parseMd "start = __-__-____ __:__ (06-06-2015 12:00 PM)"
  testDocument $ SDP.parseMd "start = __ - __ - ____ __ : __ (!`...`)"
  testDocument $ SDP.parseMd "[zip code]* = __ (12345)"
  testDocument $ SDP.parseMd "defaultless = __"
  testDocument $ SDP.parseMd "city = {BOS, SFO, NYC}"
  testDocument $ SDP.parseMd "start = __ - __ - ____"
  testDocument $ SDP.parseMd "start = __ : __"
  testDocument $ SDP.parseMd "start = __ : __ : __"
  testDocument $ SDP.parseMd "start = __ - __ - ____ __ : __ : __"
  testDocument $ SDP.parseMd "zip* = ________"
  testDocument $ SDP.parseMd "[numeric field] = #______ (23)"
  testDocument $ SDP.parseMd "i9a0qvg8* = ______ (9a0qvg8h)"
  testDocument $ SDP.parseMd "xeiodbdy  = [x] "

  C.log "All static tests passed!"

unsafeDate ∷ Int → Int → Int → DT.Date
unsafeDate y m d = unsafePartial $ M.fromJust $ join $ DT.exactDate <$> toEnum y <*> toEnum m <*> toEnum d

unsafeTime ∷ Int → Int → Int → DT.Time
unsafeTime h m s = unsafePartial $ M.fromJust $ DT.Time <$> toEnum h <*> toEnum m <*> toEnum s <*> toEnum bottom
