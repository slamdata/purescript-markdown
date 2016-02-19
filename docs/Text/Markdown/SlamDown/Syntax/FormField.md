## Module Text.Markdown.SlamDown.Syntax.FormField

#### `FormField`

``` purescript
data FormField
  = TextBox TextBoxType (Maybe (Expr String))
  | RadioButtons (Expr String) (Expr (List String))
  | CheckBoxes (Expr (List Boolean)) (Expr (List String))
  | DropDown (Expr (List String)) (Maybe (Expr String))
```

##### Instances
``` purescript
Show FormField
Eq FormField
Arbitrary FormField
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
(Eq a) => Eq (Expr a)
(Show a) => Show (Expr a)
(Arbitrary a) => Arbitrary (Expr a)
```


