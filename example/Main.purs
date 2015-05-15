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
  
data State = State String SlamDownState

data Input 
  = DocumentChanged String
  | FormFieldChanged SlamDownEvent

ui :: forall f eff. (Alternative f) => Component f Input Input
ui = render <$> stateful (State "" emptySlamDownState) update
  where
  render :: State -> H.HTML (f Input)
  render (State md form) = 
    H.div [ A.class_ (A.className "container") ]
          [ H.h2_ [ H.text "Markdown" ]
          , H.div_ [ H.textarea [ A.class_ (A.className "form-control")
                                , A.value md
                                , A.onInput (A.input DocumentChanged) 
                                ] [] ]
          , H.h2_ [ H.text "HTML Output" ]
          , H.div [ A.class_ (A.className "well") ] (output md form)
          , H.h2_ [ H.text "Form State" ]
          , H.pre_ [ H.code_ [ H.text (show form) ] ]
          ] 
          
  output :: String -> SlamDownState -> [H.HTML (f Input)]
  output md form = ((FormFieldChanged <$>) <$>) <$> renderHalogen form (parseMd md)
          
  update :: State -> Input -> State
  update (State _ form) (DocumentChanged md) = State md form
  update (State s form) (FormFieldChanged e) = State s (applySlamDownEvent form e)

main = do
  Tuple node driver <- runUI ui
  appendToBody node
  
