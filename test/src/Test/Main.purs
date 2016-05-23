module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as C
import Control.Monad.Eff.Random as Rand
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Trampoline as Trampoline

import Data.HugeNum as HN
import Data.List as L
import Data.Maybe as M
import Data.Traversable as TR
import Data.Identity as ID
import Data.Array as A
import Data.Char as CH
import Data.String as S

import Data.Tuple (uncurry)

import Text.Markdown.SlamDown.Syntax as SD
import Text.Markdown.SlamDown.Eval as SDE
import Text.Markdown.SlamDown.Parser as SDP
import Text.Markdown.SlamDown.Pretty as SDPR

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

type TestEffects e =
  ( console ∷ C.CONSOLE
  , random ∷ Rand.RANDOM
  , err ∷ Exn.EXCEPTION
  | e
  )

newtype NonEmptyString = NonEmptyString String
derive instance eqNonEmptyString ∷ Eq NonEmptyString

genChar ∷ Gen.Gen Char
genChar = Gen.elements '-' $ L.toList $ S.toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#$%^&*"

instance arbitraryNonEmptyString ∷ SC.Arbitrary NonEmptyString where
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

testDocument ∷ ∀ e. SD.SlamDownP NonEmptyString → Eff (TestEffects e) Unit
testDocument sd = do

  let printed = SDPR.prettyPrintMd sd
      parsed = SDP.parseMd printed

  C.log $ "Original: \n   " <> show sd <> "\nPrinted:\n   " <> show printed <> "\nParsed:\n   " <> show parsed
  SC.assert (parsed == sd SC.<?> "Test failed")

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

  testDocument
    $ ID.runIdentity
    $ SDE.eval
        { code: \_ _ → pure $ SD.stringValue "Evaluated code block!"
        , textBox: \t →
            case t of
              SD.PlainText _ → pure $ SD.PlainText $ pure "Evaluated plain text!"
              SD.Numeric _ → pure $ SD.Numeric $ pure $ HN.fromNumber 42.0
              SD.Date _ → pure $ SD.Date $ pure { month : 7, day : 30, year : 1992 }
              SD.Time _ → pure $ SD.Time $ pure { hours : 4, minutes : 52 }
              SD.DateTime _ →
                pure $ SD.DateTime $ pure $
                  { date : { month : 7, day : 30, year : 1992 }
                  , time : { hours : 4, minutes : 52 }
                  }
        , value: \_ → pure $ SD.stringValue "Evaluated value!"
        , list: \_ → pure $ L.singleton $ SD.stringValue "Evaluated list!"
        }
    $ SDP.parseMd
        "Some evaluated fenced code:\n\
        \\n\
        \!~~~purescript\n\
        \import Debug.Log\n\
        \\n\
        \main = log \"Hello World\"\n\
        \~~~"

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
  testDocument $ SDP.parseMd "start = __ : __ (!`...`)"
  testDocument $ SDP.parseMd "start = __-__-____ __:__ (06-06-2015 12:00 PM)"
  testDocument $ SDP.parseMd "start = __ - __ - ____ __ : __ (!`...`)"
  testDocument $ SDP.parseMd "[zip code]* = __ (12345)"
  testDocument $ SDP.parseMd "defaultless = __"
  testDocument $ SDP.parseMd "city = {BOS, SFO, NYC}"
  testDocument $ SDP.parseMd "start = __ - __ - ____"
  testDocument $ SDP.parseMd "start = __ : __"
  testDocument $ SDP.parseMd "start = __ - __ - ____ __ : __"
  testDocument $ SDP.parseMd "zip* = ________"
  testDocument $ SDP.parseMd "[numeric field] = #______ (23)"
  testDocument $ SDP.parseMd "i9a0qvg8* = ______ (9a0qvg8h)"
  testDocument $ SDP.parseMd "xeiodbdy  = [x] "

  C.log "All static tests passed!"

generated ∷ ∀ e. Eff (TestEffects e) Unit
generated = do
  C.log "Random documents"
  seed ← Rand.random
  let docs = Trampoline.runTrampoline $ Gen.sample' 10 (Gen.GenState { size: 10, seed: seed }) (SDPR.prettyPrintMd <<< runTestSlamDown <$> SC.arbitrary)
  TR.traverse C.log docs

  SC.quickCheck' 100 \(TestSlamDown sd) →
    let
      printed = SDPR.prettyPrintMd sd
      parsed = SDP.parseMd printed
    in parsed == sd SC.<?> "Pretty printer and parser incompatible for document: " <>
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
  len ← Gen.chooseInt 0.0 1.0
  Gen.vectorOf len g

smallArrayOf ∷ ∀ a. Gen.Gen a → Gen.Gen (Array a)
smallArrayOf g = do
  len ← Gen.chooseInt 1.0 2.0
  Gen.vectorOf len g

newtype TestSlamDown = TestSlamDown (SD.SlamDownP NonEmptyString)

runTestSlamDown ∷ TestSlamDown → SD.SlamDownP NonEmptyString
runTestSlamDown (TestSlamDown sd) = sd

instance arbSlamDown ∷ SC.Arbitrary TestSlamDown where
  arbitrary = (TestSlamDown <<< SD.SlamDown <<< L.toList) <$> blocks

three ∷ ∀ a. a → a → a → Array a
three a b c = [a, b, c]


blocks ∷ ∀ a. (SC.Arbitrary a, SD.Value a) ⇒ Gen.Gen (Array (SD.Block a))
blocks =
  Gen.oneOf (smallArrayOf block0)
    [ A.singleton <$> bq
    , A.singleton <$> list
    , A.singleton <$> cb
    ]
  where
  block0 ∷ Gen.Gen (SD.Block a)
  block0 =
    Gen.oneOf (SD.Paragraph <<< L.toList <$> inlines)
      [ SD.Header <$> Gen.chooseInt 1.0 6.0 <*> (L.singleton <$> simpleText)
      , SD.CodeBlock <$>
        (SD.Fenced <$> (Gen.elements true (L.singleton false)) <*>
         alphaNum)
        <*> (L.toList <$> smallArrayOf alphaNum)
      , SD.LinkReference <$> alphaNum <*> alphaNum
      , pure SD.Rule
      ]

  bq ∷ Gen.Gen (SD.Block a)
  bq = SD.Blockquote <$> (L.singleton <$> block0)

  cb ∷ Gen.Gen (SD.Block a)
  cb = SD.CodeBlock SD.Indented <<< L.toList <$> smallArrayOf alphaNum

  list ∷ Gen.Gen (SD.Block a)
  list =
    SD.Lst
      <$> Gen.oneOf (SD.Bullet <$> (Gen.elements "-" $ L.toList ["+", "*"])) [ SD.Ordered <$> (Gen.elements ")" $ L.singleton ".")]
      <*> (L.toList <$> tinyArrayOf (L.toList <$> (tinyArrayOf block0)))

inlines ∷ ∀ a. (SC.Arbitrary a, SD.Value a) ⇒ Gen.Gen (Array (SD.Inline a))
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
         <*> (Gen.elements SD.Space $ L.toList [SD.SoftBreak, SD.LineBreak])
         <*> simpleText
     , A.singleton <$> (SD.Code <$> (Gen.elements true (L.singleton false)) <*> alphaNum)
     ]

  link ∷ Gen.Gen (SD.Inline a)
  link = SD.Link <$> (L.toList <$> inlines0) <*> linkTarget

  linkTarget ∷ Gen.Gen SD.LinkTarget
  linkTarget =
    Gen.oneOf (SD.InlineLink <$> alphaNum)
      [ SD.ReferenceLink <<< M.Just <$> alphaNum ]

  formField ∷ Gen.Gen (SD.Inline a)
  formField =
    SD.FormField
      <$> alphaNum
      <*> Gen.elements true (L.singleton false)
      <*> SC.arbitrary

simpleText ∷ ∀ a. Gen.Gen (SD.Inline a)
simpleText = SD.Str <$> alphaNum

alphaNum ∷ Gen.Gen String
alphaNum = do
  len ← Gen.chooseInt 5.0 10.0
  S.fromCharArray <$> Gen.vectorOf len (Gen.elements (CH.fromCharCode 97) $ L.toList (S.toCharArray "qwertyuioplkjhgfdszxcvbnm123457890"))


main ∷ ∀ e. Eff (TestEffects e) Unit
main = do
  static
  generated
