## Module Text.Markdown.SlamDown.Syntax.Inline

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
Eq Inline
Arbitrary Inline
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


