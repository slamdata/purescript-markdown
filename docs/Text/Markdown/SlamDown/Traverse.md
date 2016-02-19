## Module Text.Markdown.SlamDown.Traverse

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


