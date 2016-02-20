## Module Text.Markdown.SlamDown.Parser

#### `validateBlock`

``` purescript
validateBlock :: forall a. Block a -> V (Array String) (Block a)
```

#### `validateSlamDown`

``` purescript
validateSlamDown :: forall a. SlamDownP a -> V (Array String) (SlamDownP a)
```

#### `parseMd`

``` purescript
parseMd :: forall a. (Value a) => String -> SlamDownP a
```


