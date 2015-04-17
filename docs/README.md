# Module Documentation

## Module Text.Markdown.SlamDown

#### `SlamDown`

``` purescript
data SlamDown
  = SlamDown [Block]
```


#### `showSlamDown`

``` purescript
instance showSlamDown :: Show SlamDown
```


#### `eqSlamDown`

``` purescript
instance eqSlamDown :: Eq SlamDown
```


#### `ordSlamDown`

``` purescript
instance ordSlamDown :: Ord SlamDown
```


#### `semigroupSlamDown`

``` purescript
instance semigroupSlamDown :: Semigroup SlamDown
```


#### `monoidSlamDown`

``` purescript
instance monoidSlamDown :: Monoid SlamDown
```


#### `Block`

``` purescript
data Block
  = Paragraph [Inline]
  | Header Number [Inline]
  | Blockquote [Block]
  | List ListType [[Block]]
  | CodeBlock CodeBlockType [String]
  | LinkReference String String
  | Rule 
```


#### `showBlock`

``` purescript
instance showBlock :: Show Block
```


#### `Inline`

``` purescript
data Inline
  = Str String
  | Entity String
  | Space 
  | SoftBreak 
  | LineBreak 
  | Emph [Inline]
  | Strong [Inline]
  | Code Boolean String
  | Link [Inline] LinkTarget
  | Image [Inline] String
  | FormField String Boolean FormField
```


#### `showInline`

``` purescript
instance showInline :: Show Inline
```


#### `ListType`

``` purescript
data ListType
  = Bullet String
  | Ordered String
```


#### `showListType`

``` purescript
instance showListType :: Show ListType
```


#### `eqListType`

``` purescript
instance eqListType :: Eq ListType
```


#### `CodeBlockType`

``` purescript
data CodeBlockType
  = Indented 
  | Fenced Boolean String
```


#### `showCodeAttr`

``` purescript
instance showCodeAttr :: Show CodeBlockType
```


#### `LinkTarget`

``` purescript
data LinkTarget
  = InlineLink String
  | ReferenceLink (Maybe String)
```


#### `showLinkTarget`

``` purescript
instance showLinkTarget :: Show LinkTarget
```


#### `Expr`

``` purescript
data Expr a
  = Literal a
  | Evaluated String
```


#### `showExpr`

``` purescript
instance showExpr :: (Show a) => Show (Expr a)
```


#### `FormField`

``` purescript
data FormField
  = TextBox TextBoxType (Expr String)
  | RadioButtons (Expr String) (Expr [String])
  | CheckBoxes (Expr [Boolean]) (Expr [String])
  | DropDown (Expr [String]) (Expr String)
```


#### `showFormField`

``` purescript
instance showFormField :: Show FormField
```


#### `TextBoxType`

``` purescript
data TextBoxType
  = PlainText 
  | Date 
  | Time 
  | DateTime 
```


#### `showTextBoxType`

``` purescript
instance showTextBoxType :: Show TextBoxType
```


#### `everywhere`

``` purescript
everywhere :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
```


#### `eval`

``` purescript
eval :: (Maybe String -> [String] -> String) -> SlamDown -> SlamDown
```



## Module Text.Markdown.SlamDown.Parser

#### `parseMd`

``` purescript
parseMd :: String -> SlamDown
```



## Module Text.Markdown.SlamDown.Pretty

#### `prettyPrintMd`

``` purescript
prettyPrintMd :: SlamDown -> String
```



## Module Text.Markdown.SlamDown.Html


This module defines functions for rendering Markdown to HTML.

#### `SlamDownEvent`

``` purescript
data SlamDownEvent
  = FormValueChanged String String
```

The type of events which can be raised by SlamDown forms

#### `markdownToHtml`

``` purescript
markdownToHtml :: String -> String
```

Convert Markdown to HTML

#### `renderHTML`

``` purescript
renderHTML :: SlamDown -> String
```

Render the SlamDown AST to a HTML `String`

#### `renderHalogen`

``` purescript
renderHalogen :: forall p f. (Alternative f) => SlamDown -> [H.HTML p (f SlamDownEvent)]
```

Render the SlamDown AST to an arbitrary Halogen HTML representation



