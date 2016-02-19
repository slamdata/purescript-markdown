## Module Text.Markdown.SlamDown.Syntax.Block

#### `Block`

``` purescript
data Block a
  = Paragraph (List (Inline a))
  | Header Int (List (Inline a))
  | Blockquote (List (Block a))
  | Lst ListType (List (List (Block a)))
  | CodeBlock CodeBlockType (List String)
  | LinkReference String String
  | Rule
```

##### Instances
``` purescript
Functor Block
(Show a) => Show (Block a)
(Eq a) => Eq (Block a)
(Arbitrary a) => Arbitrary (Block a)
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


