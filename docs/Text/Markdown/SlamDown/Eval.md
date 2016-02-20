## Module Text.Markdown.SlamDown.Eval

#### `eval`

``` purescript
eval :: forall m a. (Monad m, Value a) => { code :: String -> m String, block :: String -> List String -> m String, text :: TextBoxType -> String -> m String, value :: String -> m a, list :: String -> m (List a) } -> SlamDownP a -> m (SlamDownP a)
```


