module Main where

import Data.Void
import Data.Tuple
import Data.Either

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Control.Monad.Eff
import Control.Alternative

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A

import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Html
import Text.Markdown.SlamDown.Parser

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = do
  w <- document globalWindow
  b <- body w 
  appendChild b e
  
data State = State String

data Input 
  = DocumentChanged String
  | FormFieldChanged SlamDownEvent

ui :: forall f eff. (Alternative f) => Component f Input Input
ui = render <$> stateful (State "") update
  where
  render :: State -> H.HTML (f Input)
  render (State md) = 
    H.div [ A.class_ (A.className "container") ]
          [ H.h2_ [ H.text "Markdown" ]
          , H.div_ [ H.textarea [ A.class_ (A.className "form-control")
                                , A.value md
                                , A.onInput (A.input DocumentChanged) 
                                ] [] ]
          , H.h2_ [ H.text "HTML Output" ]
          , H.div [ A.class_ (A.className "well") ] (output md)
          ] 
          
  output :: String -> [H.HTML (f Input)]
  output md = ((FormFieldChanged <$>) <$>) <$> renderHalogen (parseMd md)
          
  update :: State -> Input -> State
  update (State _) (DocumentChanged md) = State md
  update s _ = s

main = do
  Tuple node driver <- runUI ui
  appendToBody node
  
