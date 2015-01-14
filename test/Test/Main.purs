module Test.Main where

import Debug.Trace

import qualified Data.Char as S
import qualified Data.String as S

import Data.Traversable (traverse)

import Control.Monad.Eff   
import Control.Monad.Trampoline
    
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser
import Text.Markdown.SlamDown.Pretty

import Test.StrongCheck
import Test.StrongCheck.Gen
   
foreign import exitFailure 
  "function exitFailure() {\
  \  process.exit();\
  \}" :: forall eff a. Eff eff a   
    
testDocument :: forall eff. SlamDown -> Eff (trace :: Trace | eff) Unit
testDocument sd = do
  trace "Original: "
  trace $ "  " <> show sd
  
  let printed = prettyPrintMd sd
      parsed = parseMd printed
  
  trace "Parsed: "
  trace $ "  " <> show parsed
  
  if parsed == sd
    then trace "Test passed"
    else do trace "Test failed" 
            exitFailure
    
main = do
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
                         \    import Debug.Trace\n\
                         \    \n\
                         \    main = trace \"Hello World\""
  testDocument $ parseMd "Some fenced code:\n\
                         \\n\
                         \```purescript\n\
                         \import Debug.Trace\n\
                         \\n\
                         \main = trace \"Hello World\"\n\
                         \```"
  testDocument $ parseMd "Some fenced code which can be evaluated:\n\
                         \\n\
                         \!~~~purescript\n\
                         \import Debug.Trace\n\
                         \\n\
                         \main = trace \"Hello World\"\n\
                         \~~~"
  testDocument $ eval (\_ _ -> "Evaluated!")
               $ parseMd "Some evaluated fenced code:\n\
                         \\n\
                         \!~~~purescript\n\
                         \import Debug.Trace\n\
                         \\n\
                         \main = trace \"Hello World\"\n\
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
  
  trace "All static tests passed!"
  
  trace "Some random documents:"
  
  let docs = runTrampoline $ sample 10 (prettyPrintMd <$> arbitrary)
  traverse trace docs
  
  quickCheck' 1000 \sd -> 
    let printed = prettyPrintMd sd
        parsed = parseMd printed
    in parsed == sd <?> "Pretty printer and parser incompatible for document: " <>
                        "\nOriginal: " <> show sd <>
                        "\nPrinted: " <> printed <> 
                        "\nParsed: " <> show parsed
  
deferGen :: forall a. (Unit -> Gen a) -> Gen a
deferGen g = do
  u <- pure unit
  g u   

tinyArrayOf :: forall a. Gen a -> Gen [a]
tinyArrayOf g = do
  len <- chooseInt 0 1
  vectorOf len g  
  
smallArrayOf :: forall a. Gen a -> Gen [a]
smallArrayOf g = do
  len <- chooseInt 1 2
  vectorOf len g  
  
instance arbSlamDown :: Arbitrary SlamDown where
  arbitrary = SlamDown <$> blocks
  
one :: forall a. a -> [a]  
one a = [a]      
  
three :: forall a. a -> a -> a -> [a] 
three a b c = [a, b, c]  
  
  
blocks :: Gen [Block]
blocks = oneOf (smallArrayOf block0)
               [ one <$> bq
               , one <$> list
               , one <$> cb
               ]
  where
  block0 :: Gen Block
  block0 = oneOf (Paragraph <$> inlines) 
                 [ Header <$> chooseInt 1 6 <*> (one <$> simpleText)
                 , CodeBlock <$> (Fenced <$> elements true [false] <*> alphaNum)
                             <*> smallArrayOf alphaNum
                 , pure Rule
                 ]
                 
  bq :: Gen Block
  bq = Blockquote <$> (one <$> block0)
  
  cb :: Gen Block
  cb = CodeBlock Indented <$> smallArrayOf alphaNum
  
  list :: Gen Block
  list = List <$> oneOf (Bullet <$> elements "-" ["+", "*"])
                        [ Ordered <$> elements ")" ["."] ]
              <*> tinyArrayOf (tinyArrayOf block0)

inlines :: Gen [Inline]
inlines = oneOf inlines0 [ one <$> link 
                         , one <$> formField 
                         ]
  where
  inlines0 :: Gen [Inline]
  inlines0 = oneOf (one <$> simpleText)
                  [ three <$> simpleText 
                          <*> elements Space [SoftBreak, LineBreak] 
                          <*> simpleText
                  , one <$> (Code <$> elements true [false] <*> alphaNum)
                  ]
  
  link :: Gen Inline
  link = Link <$> inlines0 <*> alphaNum
  
  formField :: Gen Inline
  formField = FormField <$> alphaNum
                        <*> elements true [false] 
                        <*> formElement
  
  formElement :: Gen FormField
  formElement = TextBox <$> elements PlainText [Date, Time, DateTime] 
                        <*> (Literal <$> alphaNum)

simpleText :: Gen Inline
simpleText = Str <$> alphaNum

alphaNum :: Gen String
alphaNum = do
  len <- chooseInt 5 10
  S.fromCharArray <$> vectorOf len (elements (S.fromCharCode 97) (S.toCharArray "qwertyuioplkjhgfdszxcvbnm123457890"))
  