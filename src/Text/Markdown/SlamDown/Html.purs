-- | This module defines functions for rendering Markdown to HTML.

module Text.Markdown.SlamDown.Html 
  ( SlamDownEvent()
  , SlamDownState(..)
  , FormFieldValue(..)
  , emptySlamDownState
  , applySlamDownEvent
  
  , markdownToHtml
  
  , renderHTML
  , renderHalogen
  ) where
    
import Data.Maybe
import Data.Array (concat, map, concatMap, zipWith)
import Data.String (joinWith)
import Data.Foldable (foldMap)
import Data.Identity

import qualified Data.StrMap as M
import qualified Data.Set as S

import Control.Alternative

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser
  
import Halogen
import Halogen.HTML.Renderer.String (renderHTMLToString)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

data FormFieldValue 
  = SingleValue TextBoxType String
  | MultipleValues (S.Set String)

instance showFormFieldValue :: Show FormFieldValue where
  show (SingleValue t s) = "(SingleValue " ++ show t ++ " " ++ show s ++ ")"
  show (MultipleValues ss) = "(MultipleValues " ++ show ss ++ ")"

-- | The state of a SlamDown form - a mapping from input keys to values
newtype SlamDownState = SlamDownState (M.StrMap FormFieldValue)

instance showSlamDownState :: Show SlamDownState where
  show (SlamDownState m) = "(SlamDownState " ++ show m ++ ")"

-- | The state of an empty form, in which all fields use their default values
emptySlamDownState :: SlamDownState
emptySlamDownState = SlamDownState M.empty

-- | The type of events which can be raised by SlamDown forms
data SlamDownEvent 
  = TextChanged TextBoxType String String
  | CheckBoxChanged String String Boolean    
    
-- | Apply a `SlamDownEvent` to a `SlamDownState`.
applySlamDownEvent :: SlamDownState -> SlamDownEvent -> SlamDownState
applySlamDownEvent (SlamDownState m) (TextChanged t key val) =
  SlamDownState (M.insert key (SingleValue t val) m)
applySlamDownEvent (SlamDownState m) (CheckBoxChanged key val checked) = 
  SlamDownState (M.alter (Just <<< updateSet) key m)
  where
  updateSet :: Maybe FormFieldValue -> FormFieldValue
  updateSet (Just (MultipleValues s)) 
    | checked = MultipleValues (S.insert val s)
    | otherwise = MultipleValues (S.delete val s) 
  updateSet _ 
    | checked = MultipleValues (S.singleton val)
    | otherwise = MultipleValues S.empty
    
-- | Convert Markdown to HTML
markdownToHtml :: SlamDownState -> String -> String
markdownToHtml st = renderHTML st <<< parseMd
   
-- | Render the SlamDown AST to a HTML `String`
renderHTML :: SlamDownState -> SlamDown -> String
renderHTML st = foldMap renderHTMLToString <<< renderHalogen_
  where
  renderHalogen_ :: SlamDown -> [H.HTML (Maybe SlamDownEvent)]
  renderHalogen_ = renderHalogen st
  
-- | Render the SlamDown AST to an arbitrary Halogen HTML representation
renderHalogen :: forall f. (Alternative f) => SlamDownState -> SlamDown -> [H.HTML (f SlamDownEvent)]
renderHalogen (SlamDownState m) (SlamDown bs) = map renderBlock bs
  where
  renderBlock :: Block -> H.HTML (f SlamDownEvent)
  renderBlock (Paragraph is) = H.p_ (map renderInline is)
  renderBlock (Header level is) = h_ level (map renderInline is)
    where
    h_ :: forall a. Number -> [H.HTML (f a)] -> H.HTML (f a)
    h_ 1 = H.h1_
    h_ 2 = H.h2_
    h_ 3 = H.h3_
    h_ 4 = H.h4_
    h_ 5 = H.h5_
    h_ 6 = H.h6_
  renderBlock (Blockquote bs) = H.blockquote_ (map renderBlock bs)
  renderBlock (List lt bss) = el_ lt (map item bss)
    where
    item :: [Block] -> H.HTML (f SlamDownEvent)
    item bs = H.li_ (map renderBlock bs)
    
    el_ :: forall a. ListType -> [H.HTML (f a)] -> H.HTML (f a)
    el_ (Bullet _)  = H.ul_
    el_ (Ordered _) = H.ol_
  renderBlock (CodeBlock _ ss) = H.pre_ [ H.code_ [ H.text (joinWith "\n" ss) ] ]
  renderBlock (LinkReference l url) = H.p_ [ H.text (l <> ": ")
                                           , H.a [ A.name l, A.id_ l, A.href url ] [ H.text url ]
                                           ]
  renderBlock Rule = H.hr_ []
  
  renderInline :: Inline -> H.HTML (f SlamDownEvent)
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
                        
  renderFormElement :: String -> FormField -> [H.HTML (f SlamDownEvent)]
  renderFormElement label (TextBox t (Literal value)) =
    [ H.input [ A.type_ "text"
              , A.id_ label
              , A.name label
              , A.value (lookupTextValue label value)
              , E.onInput (E.input (TextChanged t label))
              ] [] ]
  renderFormElement label (RadioButtons (Literal def) (Literal ls)) = 
    concatMap (\val -> radio (val == sel) val) (def : ls)
    where
    radio checked value =
      [ H.input [ A.checked checked
                , A.type_ "radio"
                , A.id_ value
                , A.name label
                , A.value value
                , E.onInput (E.input (TextChanged PlainText label))
                ] []
      , H.label [ A.for value ] [ H.text value ] 
      ]
    sel = lookupTextValue label def
  renderFormElement label (CheckBoxes (Literal bs) (Literal ls)) = 
    concat $ zipWith checkBox (lookupMultipleValues label bs ls) ls
    where
    checkBox checked value =
      [ H.input [ A.checked checked
                , A.type_ "checkbox"
                , A.id_ value
                , A.name label
                , A.value value
                , E.onChecked (E.input (CheckBoxChanged label value))
                ] []
      , H.label [ A.for value ] [ H.text value ] 
      ]
  renderFormElement label (DropDown (Literal ls) (Literal sel)) = 
    [ H.select [ A.id_ label
               , A.name label
               , E.onInput (E.input (TextChanged PlainText label))
               ] (map option ls) ]
    where
    sel' = lookupTextValue label sel
    option value = H.option [ A.selected (value == sel'), A.value value ] [ H.text value ]
  renderFormElement _ _ = [ H.text "Unsupported form element" ]

  lookupTextValue :: String -> String -> String
  lookupTextValue key def = 
    case M.lookup key m of
      Just (SingleValue _ val) -> val
      _ -> def
      
  lookupMultipleValues :: String -> [Boolean] -> [String] -> [Boolean]
  lookupMultipleValues key def ls = 
    case M.lookup key m of
      Just (MultipleValues val) -> (`S.member` val) <$> ls
      _ -> def
