## Module Text.Markdown.SlamDown

#### `SlamDown`

``` purescript
data SlamDown
  = SlamDown (List Block)
```

##### Instances
``` purescript
Show SlamDown
Eq SlamDown
Ord SlamDown
Semigroup SlamDown
Monoid SlamDown
Arbitrary SlamDown
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
Show Block
Arbitrary Block
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
Show Inline
Arbitrary Inline
```

#### `ListType`

``` purescript
data ListType
  = Bullet String
  | Ordered String
```

##### Instances
``` purescript
Show ListType
Eq ListType
Arbitrary ListType
```

#### `CodeBlockType`

``` purescript
data CodeBlockType
  = Indented
  | Fenced Boolean String
```

##### Instances
``` purescript
Show CodeBlockType
Arbitrary CodeBlockType
```

#### `LinkTarget`

``` purescript
data LinkTarget
  = InlineLink String
  | ReferenceLink (Maybe String)
```

##### Instances
``` purescript
Show LinkTarget
Arbitrary LinkTarget
```

#### `Expr`

``` purescript
data Expr a
  = Literal a
  | Unevaluated String
```

##### Instances
``` purescript
(Show a) => Show (Expr a)
(Arbitrary a) => Arbitrary (Expr a)
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
Show FormField
Arbitrary FormField
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
Show TextBoxType
Eq TextBoxType
Arbitrary TextBoxType
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
eval :: forall m. (Monad m) => { code :: String -> m String, block :: String -> List String -> m String, text :: TextBoxType -> String -> m String, value :: String -> m String, list :: String -> m (List String) } -> SlamDown -> m SlamDown
```


