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
  = TextBox TextBoxType (Maybe (Expr String))
  | RadioButtons (Expr String) (Expr [String])
  | CheckBoxes (Expr [Boolean]) (Expr [String])
  | DropDown (Expr [String]) (Maybe (Expr String))
```


#### `showFormField`

``` purescript
instance showFormField :: Show FormField
```


#### `TextBoxType`

``` purescript
data TextBoxType
  = PlainText 
  | Numeric 
  | Date 
  | Time 
  | DateTime 
```


#### `showTextBoxType`

``` purescript
instance showTextBoxType :: Show TextBoxType
```


#### `everywhereM`

``` purescript
everywhereM :: forall m. (Monad m) => (Block -> m Block) -> (Inline -> m Inline) -> SlamDown -> m SlamDown
```


#### `everywhere`

``` purescript
everywhere :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
```


#### `everywhereTopDownM`

``` purescript
everywhereTopDownM :: forall m. (Monad m) => (Block -> m Block) -> (Inline -> m Inline) -> SlamDown -> m SlamDown
```


#### `everywhereTopDown`

``` purescript
everywhereTopDown :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
```


#### `everythingM`

``` purescript
everythingM :: forall m r. (Monad m, Monoid r) => (Block -> m r) -> (Inline -> m r) -> SlamDown -> m r
```


#### `everything`

``` purescript
everything :: forall r. (Monoid r) => (Block -> r) -> (Inline -> r) -> SlamDown -> r
```


#### `eval`

``` purescript
eval :: (Maybe String -> [String] -> String) -> SlamDown -> SlamDown
```




