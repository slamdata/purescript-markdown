## Module Text.Markdown.SlamDown

#### `SlamDown`

``` purescript
data SlamDown
  = SlamDown (List Block)
```

##### Instances
``` purescript
instance showSlamDown :: Show SlamDown
instance eqSlamDown :: Eq SlamDown
instance ordSlamDown :: Ord SlamDown
instance semigroupSlamDown :: Semigroup SlamDown
instance monoidSlamDown :: Monoid SlamDown
```

#### `Block`

``` purescript
data Block
  = Paragraph (List Inline)
  | Header Int (List Inline)
  | Blockquote (List Block)
  | Lst ListType (List (List Block))
  | CodeBlock CodeBlockType (List String)
  | LinkReference String String
  | Rule
```

##### Instances
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
  | Emph (List Inline)
  | Strong (List Inline)
  | Code Boolean String
  | Link (List Inline) LinkTarget
  | Image (List Inline) String
  | FormField String Boolean FormField
```

##### Instances
``` purescript
instance showInline :: Show Inline
```

#### `ListType`

``` purescript
data ListType
  = Bullet String
  | Ordered String
```

##### Instances
``` purescript
instance showListType :: Show ListType
instance eqListType :: Eq ListType
```

#### `CodeBlockType`

``` purescript
data CodeBlockType
  = Indented
  | Fenced Boolean String
```

##### Instances
``` purescript
instance showCodeAttr :: Show CodeBlockType
```

#### `LinkTarget`

``` purescript
data LinkTarget
  = InlineLink String
  | ReferenceLink (Maybe String)
```

##### Instances
``` purescript
instance showLinkTarget :: Show LinkTarget
```

#### `Expr`

``` purescript
data Expr a
  = Literal a
  | Unevaluated String
```

##### Instances
``` purescript
instance showExpr :: (Show a) => Show (Expr a)
```

#### `FormField`

``` purescript
data FormField
  = TextBox TextBoxType (Maybe (Expr String))
  | RadioButtons (Expr String) (Expr (List String))
  | CheckBoxes (Expr (List Boolean)) (Expr (List String))
  | DropDown (Expr (List String)) (Maybe (Expr String))
```

##### Instances
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

##### Instances
``` purescript
instance showTextBoxType :: Show TextBoxType
instance eqTextBoxType :: Eq TextBoxType
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
eval :: forall m. (Monad m) => (Maybe String -> List String -> m String) -> (TextBoxType -> String -> m String) -> (String -> m String) -> (String -> m (List String)) -> SlamDown -> m SlamDown
```

Evaluates code blocks embedded within a SlamDown document.
- The first function handles the evaluation of fenced code blocks and
  inline code values. The first argument is the type associated with a
  fenced code block, taken from the opening triple-backtick. The second
  argument is each line of the code block. In the case of inline code the
  first argument is always `Nothing` and the `List` will only contain one
  item.
- The second function handles code blocks used to provide a default value
  for a text-based input.
- The third function handles singular values used in determining the
  selected value in a dropdown or collection of radio buttons.
- The fourth function handles lists of values used for the list of options
  in a dropdown, the list of values for checkboxes, and the list of
  selected values for checkboxes.


