## Module Text.Markdown.SlamDown.Syntax.Inline

#### `Inline`

``` purescript
data Inline a
  = Str String
  | Entity String
  | Space
  | SoftBreak
  | LineBreak
  | Emph (List (Inline a))
  | Strong (List (Inline a))
  | Code Boolean String
  | Link (List (Inline a)) LinkTarget
  | Image (List (Inline a)) String
  | FormField String Boolean (FormField a)
```

##### Instances
``` purescript
Functor Inline
(Show a) => Show (Inline a)
(Eq a) => Eq (Inline a)
(Arbitrary a) => Arbitrary (Inline a)
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
Eq LinkTarget
Arbitrary LinkTarget
```


