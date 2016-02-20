## Module Text.Markdown.SlamDown.Syntax

#### `SlamDownP`

``` purescript
data SlamDownP a
  = SlamDown (List (Block a))
```

`SlamDownP` is the type of SlamDown abstract syntax trees which take values in `a`.

##### Instances
``` purescript
Functor SlamDownP
(Show a) => Show (SlamDownP a)
(Eq a) => Eq (SlamDownP a)
(Show a, Eq a) => Ord (SlamDownP a)
Semigroup (SlamDownP a)
Monoid (SlamDownP a)
(Arbitrary a) => Arbitrary (SlamDownP a)
```

#### `SlamDown`

``` purescript
type SlamDown = SlamDownP String
```


