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

#### `showFormFieldValue`

``` purescript
instance showFormFieldValue :: Show FormFieldValue
```


#### `SlamDownState`

``` purescript
newtype SlamDownState
```

The state of a SlamDown form - a mapping from input keys to values

#### `showSlamDownState`

``` purescript
instance showSlamDownState :: Show SlamDownState
```


#### `emptySlamDownState`

``` purescript
emptySlamDownState :: SlamDownState
```

The state of an empty form, in which all fields use their default values

#### `SlamDownEvent`

``` purescript
data SlamDownEvent
```

The type of events which can be raised by SlamDown forms

#### `applySlamDownEvent`

``` purescript
applySlamDownEvent :: SlamDownState -> SlamDownEvent -> SlamDownState
```

Apply a `SlamDownEvent` to a `SlamDownState`.

#### `markdownToHtml`

``` purescript
markdownToHtml :: SlamDownState -> String -> String
```

Convert Markdown to HTML

#### `renderHTML`

``` purescript
renderHTML :: SlamDownState -> SlamDown -> String
```

Render the SlamDown AST to a HTML `String`

#### `renderHalogen`

``` purescript
renderHalogen :: forall f. (Alternative f) => SlamDownState -> SlamDown -> [H.HTML (f SlamDownEvent)]
```

Render the SlamDown AST to an arbitrary Halogen HTML representation



