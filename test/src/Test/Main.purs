module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import Control.Monad.Eff.Random as Rand
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Trampoline as Trampoline

import Data.Array as A
import Data.Char as CH
import Data.DateTime as DT
import Data.Either (Either(..), isLeft)
import Data.Enum (toEnum)
import Data.HugeNum as HN
import Data.Identity as ID
import Data.List as L
import Data.Maybe as M
import Data.Newtype (un)
import Data.String as S
import Data.Traversable as TR

import Data.Tuple (uncurry)

import Text.Markdown.SlamDown.Syntax as SD
import Text.Markdown.SlamDown.Eval as SDE
import Text.Markdown.SlamDown.Parser as SDP
import Text.Markdown.SlamDown.Pretty as SDPR

import Test.StrongCheck as SC
import Test.StrongCheck.Arbitrary as SCA
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.LCG as LCG

import Partial.Unsafe (unsafePartial)

type TestEffects e =
  ( console ∷ C.CONSOLE
  , random ∷ Rand.RANDOM
  , exception ∷ Exn.EXCEPTION
  | e
  )

newtype NonEmptyString = NonEmptyString String
derive instance eqNonEmptyString ∷ Eq NonEmptyString
derive instance ordNonEmptyString ∷ Ord NonEmptyString

genChar ∷ Gen.Gen Char
genChar = Gen.elements '-' $ L.fromFoldable $ S.toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 @!#$%^"

instance arbitraryNonEmptyString ∷ SCA.Arbitrary NonEmptyString where
  arbitrary =
    Gen.arrayOf1 genChar
      <#> uncurry A.cons
      >>> S.fromCharArray
      >>> S.trim
      >>> NonEmptyString

instance showNonEmptyString ∷ Show NonEmptyString where
  show (NonEmptyString str) = str

instance valueNonEmptyString ∷ SD.Value NonEmptyString where
  stringValue = NonEmptyString
  renderValue (NonEmptyString str) = str

testDocument ∷ ∀ e. Either String (SD.SlamDownP NonEmptyString) → Eff (TestEffects e) Unit
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
  SC.assert (parsed == sd SC.<?> "Test failed")

failDocument ∷ ∀ e. Either String (SD.SlamDownP NonEmptyString) → Eff (TestEffects e) Unit
failDocument sd = SC.assert (isLeft sd SC.<?> "Test failed")

static ∷ ∀ e. Eff (TestEffects e) Unit
static = do
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

generated ∷ ∀ e. Eff (TestEffects e) Unit
generated = do
  C.log "Random documents"
  seed ← LCG.randomSeed
  let
    docs =
      Trampoline.runTrampoline
        $ Gen.sample'
            10 (Gen.GenState { size: 10, seed })
            (SDPR.prettyPrintMd <<< runTestSlamDown <$> SCA.arbitrary)

  _ ← TR.traverse C.log docs

  SC.quickCheck' 100 \(TestSlamDown sd) →
    let
      printed = SDPR.prettyPrintMd sd
      parsed = SDP.parseMd printed
    in parsed == (Right sd) SC.<?> "Pretty printer and parser incompatible for document: " <>
      "\nOriginal: \n" <> show sd <>
      "\nPrinted: \n" <> printed <>
      "\nParsed: \n" <> show parsed
  C.log "All dynamic passed"

deferGen ∷ ∀ a. (Unit → Gen.Gen a) → Gen.Gen a
deferGen g = do
  u ← pure unit
  g u

tinyArrayOf ∷ ∀ a. Gen.Gen a → Gen.Gen (Array a)
tinyArrayOf g = do
  len ← Gen.chooseInt 0 1
  Gen.vectorOf len g

smallArrayOf ∷ ∀ a. Gen.Gen a → Gen.Gen (Array a)
smallArrayOf g = do
  len ← Gen.chooseInt 1 2
  Gen.vectorOf len g

newtype TestSlamDown = TestSlamDown (SD.SlamDownP NonEmptyString)

runTestSlamDown ∷ TestSlamDown → SD.SlamDownP NonEmptyString
runTestSlamDown (TestSlamDown sd) = sd

instance arbSlamDown ∷ SCA.Arbitrary TestSlamDown where
  arbitrary = (TestSlamDown <<< SD.SlamDown <<< L.fromFoldable) <$> blocks

three ∷ ∀ a. a → a → a → Array a
three a b c = [a, b, c]


blocks ∷ ∀ a. SCA.Arbitrary a ⇒ SD.Value a ⇒ Gen.Gen (Array (SD.Block a))
blocks =
  Gen.oneOf (smallArrayOf block0)
    [ A.singleton <$> bq
    , A.singleton <$> list
    , A.singleton <$> cb
    ]
  where
  block0 ∷ Gen.Gen (SD.Block a)
  block0 =
    Gen.oneOf (SD.Paragraph <<< L.fromFoldable <$> inlines)
      [ SD.Header <$> Gen.chooseInt 1 6 <*> (L.singleton <$> simpleText)
      , SD.CodeBlock <$>
        (SD.Fenced <$> (Gen.elements true (L.singleton false)) <*>
         alphaNum)
        <*> (L.fromFoldable <$> smallArrayOf alphaNum)
      , SD.LinkReference <$> alphaNum <*> alphaNum
      , pure SD.Rule
      ]

  bq ∷ Gen.Gen (SD.Block a)
  bq = SD.Blockquote <$> (L.singleton <$> block0)

  cb ∷ Gen.Gen (SD.Block a)
  cb = SD.CodeBlock SD.Indented <<< L.fromFoldable <$> smallArrayOf alphaNum

  list ∷ Gen.Gen (SD.Block a)
  list =
    SD.Lst
      <$> Gen.oneOf (SD.Bullet <$> (Gen.elements "-" $ L.fromFoldable ["+", "*"])) [ SD.Ordered <$> (Gen.elements ")" $ L.singleton ".")]
      <*> (L.fromFoldable <$> tinyArrayOf (L.fromFoldable <$> (tinyArrayOf block0)))

inlines ∷ ∀ a. SCA.Arbitrary a ⇒ SD.Value a ⇒ Gen.Gen (Array (SD.Inline a))
inlines =
  Gen.oneOf inlines0
    [ A.singleton <$> link
    , A.singleton <$> formField
    ]
  where
  inlines0 ∷ Gen.Gen (Array (SD.Inline a))
  inlines0 =
    Gen.oneOf (A.singleton <$> simpleText)
     [ three
         <$> simpleText
         <*> (Gen.elements SD.Space $ L.fromFoldable [SD.SoftBreak, SD.LineBreak])
         <*> simpleText
     , A.singleton <$> (SD.Code <$> (Gen.elements true (L.singleton false)) <*> alphaNum)
     ]

  link ∷ Gen.Gen (SD.Inline a)
  link = SD.Link <$> (L.fromFoldable <$> inlines0) <*> linkTarget

  linkTarget ∷ Gen.Gen SD.LinkTarget
  linkTarget =
    Gen.oneOf (SD.InlineLink <$> alphaNum)
      [ SD.ReferenceLink <<< M.Just <$> alphaNum ]

  formField ∷ Gen.Gen (SD.Inline a)
  formField =
    SD.FormField
      <$> alphaNum
      <*> Gen.elements true (L.singleton false)
      <*> SCA.arbitrary

simpleText ∷ ∀ a. Gen.Gen (SD.Inline a)
simpleText = SD.Str <$> alphaNum

alphaNum ∷ Gen.Gen String
alphaNum = do
  len ← Gen.chooseInt 5 10
  S.fromCharArray <$> Gen.vectorOf len (Gen.elements (CH.fromCharCode 97) $ L.fromFoldable (S.toCharArray "qwertyuioplkjhgfdszxcvbnm123457890ąćęóśźżĄĆĘÓŚŹŻ"))


main ∷ ∀ e. Eff (TestEffects e) Unit
main = do
  static
  generated
