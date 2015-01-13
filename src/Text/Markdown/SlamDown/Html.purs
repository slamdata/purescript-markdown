module Text.Markdown.SlamDown.Html 
  ( Html()
  , markdownToHtml
  , renderHtml
  , toHtml
  ) where
    
import Data.Array (map, concatMap, zipWith)
import Data.String (joinWith)
import Data.Foldable (foldMap)
    
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser
    
data Html 
  = Text String 
  | Open String [Attribute] [Html] 
  | Closed String
    
data Attribute = Attribute String String

markdownToHtml :: String -> String
markdownToHtml = foldMap renderHtml <<< toHtml <<< parseMd

renderHtml :: Html -> String
renderHtml (Text s) = s
renderHtml (Open nm attrs content) = open <> inside <> close
  where
  open   = "<"  <> nm <> joinWith "" (map renderAttr attrs) <> ">"
  close  = "</" <> nm <> ">"
  inside = joinWith "" $ map renderHtml content
      
  renderAttr (Attribute k v) = " " <> k <> "=" <> show v
renderHtml (Closed nm) = "<"  <> nm <> ">"
    
toHtml :: SlamDown -> [Html]
toHtml (SlamDown bs) = map renderBlock bs
  where
  renderBlock :: Block -> Html
  renderBlock (Paragraph is) = Open "p" [] (map renderInline is)
  renderBlock (Header level is) = Open ("h" <> show level) [] (map renderInline is)
  renderBlock (Blockquote bs) = Open "blockquote" [] (map renderBlock bs)
  renderBlock (List lt bss) = Open (el lt) [] (map item bss)
    where
    item bs = li (map renderBlock bs)    
        
    li es = Open "li" [] es
    
    el (Bullet _)  = "ul"
    el (Ordered _) = "ol"
  renderBlock (CodeBlock _ ss) = Open "pre" [] [Open "code" [] [Text (joinWith "\n" ss)]]
  renderBlock Rule = Closed "hr"
  
  renderInline :: Inline -> Html
  renderInline (Str s) = Text s 
  renderInline (Entity s) = Text s
  renderInline Space = Text " "
  renderInline SoftBreak = Text "\n"
  renderInline LineBreak = Closed "br"
  renderInline (Emph is) = Open "em" [] (map renderInline is)
  renderInline (Strong is) = Open "strong" [] (map renderInline is)
  renderInline (Code _ c) = Open "code" [] [Text c]
  renderInline (Link body url) = Open "a" [Attribute "href" url] (map renderInline body)
  renderInline (Image body url) = Open "img" [Attribute "src" url] (map renderInline body)
  renderInline (FormField label req el) = Open "label" [Attribute "for" label] $ Text label : requiredLabel (renderFormElement label el)
    where
    requiredLabel els | req = Text "*" : els
                      | otherwise = els
                      
  renderFormElement :: String -> FormField -> [Html]
  renderFormElement label (TextBox (Literal value)) = 
    [ Open "input" [ Attribute "type" "text"
                   , Attribute "id" label
                   , Attribute "name" label
                   , Attribute "value" value
                   ] [] ]
  renderFormElement label (RadioButtons (Literal def) (Literal ls)) = 
    radio true def : map (radio false) ls
    where
    radio checked value =
      Open "input" (checkedAttribute 
                      [ Attribute "type" "radio"
                      , Attribute "id" value
                      , Attribute "name" label
                      , Attribute "value" value
                      ]) 
                   [ Open "label" [Attribute "for" value] [Text value] ]
      where
      checkedAttribute xs | checked = Attribute "checked" "checked" : xs
                          | otherwise = xs
  renderFormElement label (CheckBoxes (Literal bs) (Literal ls)) = 
    zipWith checkBox bs ls
    where
    checkBox checked value =
      Open "input" (checkedAttribute 
                      [ Attribute "type" "checkbox"
                      , Attribute "id" value
                      , Attribute "name" label
                      , Attribute "value" value
                      ])
                   [ Open "label" [Attribute "for" value] [Text value] ]
      where
      checkedAttribute xs | checked = Attribute "checked" "checked" : xs
                          | otherwise = xs
  renderFormElement label (DropDown (Literal ls) (Literal sel)) = 
    [ Open "select" [ Attribute "id" label
                    , Attribute "name" label
                    ] (map option ls) ]
    where
    option value = Open "option" (selectedAttribute [ Attribute "value" value ]) [ Text value ]
      where
      selectedAttribute xs | value == sel = Attribute "selected" "selected" : xs
                           | otherwise = xs
  renderFormElement _ _ = [Text "Unsupported form element"]