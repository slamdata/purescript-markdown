## Module Text.Markdown.SlamDown.Syntax.FormField

#### `FormField`

``` purescript
data FormField a
  = TextBox TextBoxType (Maybe (Expr String))
  | RadioButtons (Expr a) (Expr (List a))
  | CheckBoxes (Expr (List Boolean)) (Expr (List a))
  | DropDown (Expr (List a)) (Maybe (Expr a))
```

##### Instances
``` purescript
Functor FormField
(Show a) => Show (FormField a)
(Eq a) => Eq (FormField a)
(Arbitrary a) => Arbitrary (FormField a)
```

#### `TextBoxType`

``` purescript
data TextBoxType
  = PlainText
  | Numeric
  | Date
  | Time
  | DateTime
```

##### Instances
``` purescript
Show TextBoxType
Eq TextBoxType
Arbitrary TextBoxType
```

#### `Expr`

``` purescript
data Expr a
  = Literal a
  | Unevaluated String
```

##### Instances
``` purescript
Functor Expr
(Eq a) => Eq (Expr a)
(Show a) => Show (Expr a)
(Arbitrary a) => Arbitrary (Expr a)
```


