module Main where

import Data.Void
import Data.Tuple
import Data.Either
import Data.Bifunctor (rmap)

import Control.Monad.Eff

import DOM

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

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node
  
data State = State String

data Input 
  = DocumentChanged String
  | FormFieldChanged SlamDownEvent

ui :: forall p f eff. (Alternative f) => Component p f Input Input
ui = component (render <$> stateful (State "") update)
  where
  render :: State -> H.HTML p (f Input)
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
          
  output :: String -> [H.HTML p (f Input)]
  output md = rmap (FormFieldChanged <$>) <$> renderHalogen (parseMd md)
          
  update :: State -> Input -> State
  update (State _) (DocumentChanged md) = State md
  update s _ = s

main = do
  Tuple node driver <- runUI ui
  appendToBody node
  
