## Module Text.Markdown.SlamDown.Eval

#### `eval`

``` purescript
eval :: forall m. (Monad m) => { code :: String -> m String, block :: String -> List String -> m String, text :: TextBoxType -> String -> m String, value :: String -> m String, list :: String -> m (List String) } -> SlamDown -> m SlamDown
```


