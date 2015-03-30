-- | This module defines functions for rendering Markdown to HTML.

module Text.Markdown.SlamDown.Html 
  ( SlamDownEvent(..)
  
  , markdownToHtml
  
  , renderHTML
  , renderHalogen
  ) where
    
import Data.Maybe (maybe)
import Data.Array (map, concatMap, zipWith)
import Data.String (joinWith)
import Data.Foldable (foldMap)
import Data.Identity

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser
  
import Halogen
import Halogen.HTML.Renderer.String (HTML(), renderHTMLToString)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events.Forms as E

-- | The type of events which can be raised by SlamDown forms
data SlamDownEvent = FormValueChanged String String
    
-- | Convert Markdown to HTML
markdownToHtml :: String -> String
markdownToHtml = renderHTML <<< parseMd
   
-- | Render the SlamDown AST to a HTML `String`
renderHTML :: SlamDown -> String
renderHTML (SlamDown bs) = foldMap (renderHTMLToString <<< renderBlock') bs
  where    
  renderBlock' :: forall p f. Block -> HTML p (Identity SlamDownEvent)
  renderBlock' = renderBlock
  
-- | Render the SlamDown AST to an arbitrary Halogen HTML representation
renderHalogen :: forall p i f node. (Applicative f, H.HTMLRepr node) => SlamDown -> [node p (f SlamDownEvent)]
renderHalogen (SlamDown bs) = map renderBlock bs

renderBlock :: forall p i f node. (Applicative f, H.HTMLRepr node) => Block -> node p (f SlamDownEvent)
renderBlock (Paragraph is) = H.p_ (map renderInline is)
renderBlock (Header level is) = h_ level (map renderInline is)
  where
  h_ :: forall a. Number -> [node p a] -> node p a
  h_ 1 = H.h1_
  h_ 2 = H.h2_
  h_ 3 = H.h3_
  h_ 4 = H.h4_
  h_ 5 = H.h5_
  h_ 6 = H.h6_
renderBlock (Blockquote bs) = H.blockquote_ (map renderBlock bs)
renderBlock (List lt bss) = el_ lt (map item bss)
  where
  item :: [Block] -> node p (f SlamDownEvent)
  item bs = H.li_ (map renderBlock bs)
  
  el_ :: forall a.ListType -> [node p a] -> node p a
  el_ (Bullet _)  = H.ul_
  el_ (Ordered _) = H.ol_
renderBlock (CodeBlock _ ss) = H.pre_ [ H.code_ [ H.text (joinWith "\n" ss) ] ]
renderBlock (LinkReference l url) = H.p_ [ H.text (l <> ": ")
                                         , H.a [ A.name l, A.id_ l, A.href url ] [ H.text url ]
                                         ]
renderBlock Rule = H.hr_ []

renderInline :: forall p i f node. (Applicative f, H.HTMLRepr node) => Inline -> node p (f SlamDownEvent)
renderInline (Str s) = H.text s 
renderInline (Entity s) = H.text s
renderInline Space = H.text " "
renderInline SoftBreak = H.text "\n"
renderInline LineBreak = H.br_ []
renderInline (Emph is) = H.em_ (map renderInline is)
renderInline (Strong is) = H.strong_ (map renderInline is)
renderInline (Code _ c) = H.code_ [ H.text c ]
renderInline (Link body tgt) = H.a [ A.href (href tgt) ] (map renderInline body)
  where
  href (InlineLink url) = url
  href (ReferenceLink tgt) = maybe "" ((<>) "#") tgt
renderInline (Image body url) = H.img [ A.src url ] (map renderInline body)
renderInline (FormField label req el) = H.label [ A.for label ] $ H.text label : requiredLabel (renderFormElement label el)
  where
  requiredLabel els | req = H.text "*" : els
                    | otherwise = els
                      
renderFormElement :: forall p i f node. (Applicative f, H.HTMLRepr node) => String -> FormField -> [node p (f SlamDownEvent)]
renderFormElement label (TextBox _ (Literal value)) = 
  [ H.input [ A.type_ "text"
            , A.id_ label
            , A.name label
            , A.value value
            , E.onValueChanged (pure <<< pure <<< FormValueChanged label)
            ] [] ]
renderFormElement label (RadioButtons (Literal def) (Literal ls)) = 
  radio true def : map (radio false) ls
  where
  radio checked value =
    H.input [ A.checked checked
            , A.type_ "radio"
            , A.id_ value
            , A.name label
            , A.value value
            , E.onValueChanged (pure <<< pure <<< FormValueChanged label)
            ]
            [ H.label [ A.for value ] [ H.text value ] ]
renderFormElement label (CheckBoxes (Literal bs) (Literal ls)) = 
  zipWith checkBox bs ls
  where
  checkBox checked value =
    H.input [ A.checked checked
            , A.type_ "checkbox"
            , A.id_ value
            , A.name label
            , A.value value
            , E.onValueChanged (pure <<< pure <<< FormValueChanged label)
            ]
            [ H.label [ A.for value ] [ H.text value ] ]
renderFormElement label (DropDown (Literal ls) (Literal sel)) = 
  [ H.select [ A.id_ label
             , A.name label
             , E.onValueChanged (pure <<< pure <<< FormValueChanged label)
             ] (map option ls) ]
  where
  option value = H.option [ A.selected (value == sel), A.value value ] [ H.text value ]
renderFormElement _ _ = [ H.text "Unsupported form element" ]
