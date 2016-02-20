## Module Text.Markdown.SlamDown.Traverse

#### `everywhereM`

``` purescript
everywhereM :: forall m a. (Monad m) => (Block a -> m (Block a)) -> (Inline a -> m (Inline a)) -> SlamDownP a -> m (SlamDownP a)
```

#### `everywhere`

``` purescript
everywhere :: forall a. (Block a -> Block a) -> (Inline a -> Inline a) -> SlamDownP a -> SlamDownP a
```

#### `everywhereTopDownM`

``` purescript
everywhereTopDownM :: forall m a. (Monad m) => (Block a -> m (Block a)) -> (Inline a -> m (Inline a)) -> SlamDownP a -> m (SlamDownP a)
```

#### `everywhereTopDown`

``` purescript
everywhereTopDown :: forall a. (Block a -> Block a) -> (Inline a -> Inline a) -> SlamDownP a -> SlamDownP a
```

#### `everythingM`

``` purescript
everythingM :: forall m a r. (Monad m, Monoid r) => (Block a -> m r) -> (Inline a -> m r) -> SlamDownP a -> m r
```

#### `everything`

``` purescript
everything :: forall r a. (Monoid r) => (Block a -> r) -> (Inline a -> r) -> SlamDownP a -> r
```


