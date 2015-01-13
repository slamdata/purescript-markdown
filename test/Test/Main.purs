module Test.Main where

import Debug.Trace

import Control.Monad.Eff   
    
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser
import Text.Markdown.SlamDown.Pretty
   
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
                         
  trace "All tests passed!"