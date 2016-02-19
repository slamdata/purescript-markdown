## Module Text.Markdown.SlamDown.Syntax.Block

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
Eq Block
Arbitrary Block
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
Eq CodeBlockType
Arbitrary CodeBlockType
```


