## Module Text.Markdown.SlamDown.Syntax.Value

#### `Value`

``` purescript
class (Eq a) <= Value a where
  stringValue :: String -> a
  renderValue :: a -> String
```

##### Instances
``` purescript
Value String
```


