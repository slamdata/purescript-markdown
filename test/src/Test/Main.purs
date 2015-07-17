module Test.Main where

import Prelude 
import Data.Maybe
import Data.List

import qualified Data.Array as A
import qualified Data.Char as S
import qualified Data.String as S

import Data.Traversable (traverse)

import Control.Monad.Eff   
import Control.Monad.Eff.Console
import Control.Monad.Eff.Random
import Control.Monad.Trampoline
    
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser
import Text.Markdown.SlamDown.Pretty

import Test.StrongCheck
import Test.StrongCheck.Gen
   
foreign import inPhantom :: forall e. Eff e Unit -> Eff e Unit
    
testDocument :: forall eff. SlamDown -> Eff _ Unit
testDocument sd = do
  log "Original: "
  log $ "  " <> show sd
  
  let printed = prettyPrintMd sd
      parsed = parseMd printed
  
  log "Parsed: "
  log $ "  " <> show parsed

  assert (parsed == sd <?> "Test failed")

static :: Eff _ Unit 
static = do
  testDocument $ parseMd "Paragraph"
  testDocument $ parseMd "Paragraph with spaces"
  testDocument $ parseMd "Paragraph with an entity: &copy;"
  testDocument $ parseMd "Paragraph with a [link](http://purescript.org)"
  testDocument $ parseMd "Paragraph with an ![image](image.png)"
  testDocument $ parseMd "Paragraph with some `embedded code`"
  testDocument $ parseMd "Paragraph with some !`code which can be evaluated`"
  testDocument $ parseMd "Paragraph with _emphasis_"
  testDocument $ parseMd "Paragraph with _emphasis_ and __strong text__"
  testDocument $ parseMd "Paragraph with a\n\
                         \soft break"
  testDocument $ parseMd "Paragraph with a  \n\
                         \line break"
  testDocument $ parseMd "Two\n\
                         \\n\
                         \paragraphs"
  testDocument $ parseMd "Header\n\
                         \==="
  testDocument $ parseMd "# Header\n\
                         \\n\
                         \Paragraph text"
  testDocument $ parseMd "## Header\n\
                         \\n\
                         \Paragraph text"
  testDocument $ parseMd "### Header\n\
                         \\n\
                         \Paragraph text"
  testDocument $ parseMd "#### Header\n\
                         \\n\
                         \Paragraph text"
  testDocument $ parseMd "##### Header\n\
                         \\n\
                         \Paragraph text"
  testDocument $ parseMd "###### Header\n\
                         \\n\
                         \Paragraph text"
  testDocument $ parseMd "Rule:\n\
                         \\n\
                         \-----"
  testDocument $ parseMd "A blockquote:\n\
                         \\n\
                         \> Here is some text\n\
                         \> inside a blockquote"
  testDocument $ parseMd "A nested blockquote:\n\
                         \\n\
                         \> Here is some text\n\
                         \> > Here is some more text"
  testDocument $ parseMd "An unordered list:\n\
                         \\n\
                         \* Item 1\n\
                         \* Item 2"
  testDocument $ parseMd "An ordered list:\n\
                         \\n\
                         \1. Item 1\n\
                         \1. Item 2"
  testDocument $ parseMd "A nested list:\n\
                         \\n\
                         \1. Item 1\n\
                         \1. 1. Item 2\n\
                         \   1. Item 3"
  testDocument $ parseMd "Some indented code:\n\
                         \\n\
                         \    import Debug.Log\n\
                         \    \n\
                         \    main = log \"Hello World\""
  testDocument $ parseMd "Some fenced code:\n\
                         \\n\
                         \```purescript\n\
                         \import Debug.Log\n\
                         \\n\
                         \main = log \"Hello World\"\n\
                         \```"
  testDocument $ parseMd "Some fenced code which can be evaluated:\n\
                         \\n\
                         \!~~~purescript\n\
                         \import Debug.Log\n\
                         \\n\
                         \main = log \"Hello World\"\n\
                         \~~~"
  testDocument $ eval (\_ _ -> "Evaluated!")
               $ parseMd "Some evaluated fenced code:\n\
                         \\n\
                         \!~~~purescript\n\
                         \import Debug.Log\n\
                         \\n\
                         \main = log \"Hello World\"\n\
                         \~~~"
  testDocument $ parseMd "name = __ (Phil Freeman)"
  testDocument $ parseMd "name = __ (!`name`)"
  testDocument $ parseMd "sex* = (x) male () female () other"
  testDocument $ parseMd "sex* = (!`def`) !`others`"
  testDocument $ parseMd "city = {BOS, SFO, NYC} (NYC)"
  testDocument $ parseMd "city = {!`...`} (!`...`)"
  testDocument $ parseMd "phones = [] Android [x] iPhone [x] Blackberry"
  testDocument $ parseMd "phones = [!`...`] !`...`"
  testDocument $ parseMd "start = __ - __ - ____ (06-06-2015)"
  testDocument $ parseMd "start = __ - __ - ____ (!`...`)"
  testDocument $ parseMd "start = __ : __ (10:32 PM)"
  testDocument $ parseMd "start = __ : __ (!`...`)"
  testDocument $ parseMd "start = __ - __ - ____ __ : __ (06-06-2015 12:00 PM)"
  testDocument $ parseMd "start = __ - __ - ____ __ : __ (!`...`)"
  testDocument $ parseMd "[zip code]* = __ (12345)"
  testDocument $ parseMd "defaultless = __"
  testDocument $ parseMd "city = {BOS, SFO, NYC}"
  testDocument $ parseMd "start = __ - __ - ____"
  testDocument $ parseMd "start = __ : __"
  testDocument $ parseMd "start = __ - __ - ____ __ : __"
  testDocument $ parseMd "zip* = ________"
  testDocument $ parseMd "[numeric field] = #______"
  
  log "All static tests passed!"

generated :: Eff _ Unit 
generated = do
  log "Random documents"
  seed <- random
  let docs = runTrampoline $ sample' 10 (GenState { size: 10, seed: seed }) (prettyPrintMd <$> arbitrary)
  traverse log docs

  quickCheck' 100 \sd -> 
    let printed = prettyPrintMd sd
        parsed = parseMd printed
    in parsed == sd <?> "Pretty printer and parser incompatible for document: " <>
      "\nOriginal: " <> show sd <>
      "\nPrinted: " <> printed <>
      "\nParsed: " <> show parsed
  log "All dynamic passed"

deferGen :: forall a. (Unit -> Gen a) -> Gen a
deferGen g = do
  u <- pure unit
  g u   

tinyArrayOf :: forall a. Gen a -> Gen (Array a)
tinyArrayOf g = do
  len <- chooseInt 0.0 1.0
  vectorOf len g  
  
smallArrayOf :: forall a. Gen a -> Gen (Array a)
smallArrayOf g = do
  len <- chooseInt 1.0 2.0
  vectorOf len g  
  
instance arbSlamDown :: Arbitrary SlamDown where
  arbitrary = (SlamDown <<< toList) <$> blocks
  
three :: forall a. a -> a -> a -> Array a
three a b c = [a, b, c]  
  
  
blocks :: Gen (Array Block)
blocks = oneOf (smallArrayOf block0)
               [ A.singleton <$> bq
               , A.singleton <$> list
               , A.singleton <$> cb
               ]
  where
  block0 :: Gen Block
  block0 = oneOf (Paragraph <<< toList <$> inlines) 
                 [ Header <$> chooseInt 1.0 6.0 <*> (singleton <$> simpleText)
                 , CodeBlock <$>
                   (Fenced <$> (elements true (singleton false)) <*>
                    alphaNum)
                   <*> (toList <$> smallArrayOf alphaNum)
                 , LinkReference <$> alphaNum <*> alphaNum
                 , pure Rule
                 ]
                 
  bq :: Gen Block
  bq = Blockquote <$> (singleton <$> block0)
  
  cb :: Gen Block
  cb = (CodeBlock Indented <<< toList) <$> smallArrayOf alphaNum
  
  list :: Gen Block
  list = Lst <$> oneOf (Bullet <$> (elements "-" $ toList ["+", "*"]))
                        [ Ordered <$> (elements ")" $ singleton ".")]
              <*> (toList <$> tinyArrayOf (toList <$> (tinyArrayOf block0)))

inlines :: Gen (Array Inline)
inlines = oneOf inlines0 [ A.singleton <$> link
                         , A.singleton <$> formField
                         ]
  where
  inlines0 :: Gen (Array Inline)
  inlines0 = oneOf (A.singleton <$> simpleText)
                  [ three <$> simpleText 
                          <*> (elements Space $ toList [SoftBreak, LineBreak])
                          <*> simpleText
                  , A.singleton <$> (Code <$> (elements true (singleton false)) <*> alphaNum)
                  ]
  
  link :: Gen Inline
  link = Link <$> (toList <$> inlines0) <*> linkTarget
  
  linkTarget :: Gen LinkTarget
  linkTarget = oneOf (InlineLink <$> alphaNum)
                     [ ReferenceLink <<< Just <$> alphaNum ]
  
  formField :: Gen Inline
  formField = FormField <$> alphaNum
                        <*> elements true (singleton false)
                        <*> formElement
  
  formElement :: Gen FormField
  formElement = TextBox <$> (elements PlainText $ toList [Date, Time, DateTime])
                        <*> (Just <<< Literal <$> alphaNum)

simpleText :: Gen Inline
simpleText = Str <$> alphaNum

alphaNum :: Gen String
alphaNum = do
  len <- chooseInt 5.0 10.0
  S.fromCharArray <$> vectorOf len (elements (S.fromCharCode 97) $ toList (S.toCharArray "qwertyuioplkjhgfdszxcvbnm123457890"))


main :: Eff _ Unit 
main = inPhantom do
  static 
  generated
