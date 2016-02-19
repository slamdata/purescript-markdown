module Text.Markdown.SlamDown.Syntax.FormField
  ( FormField(..)
  , TextBoxType(..)
  , Expr(..)
  ) where

import Prelude
import Data.List as L
import Data.Maybe as M

import Test.StrongCheck as SC
import Test.StrongCheck.Gen as SC

data FormField
  = TextBox TextBoxType (M.Maybe (Expr String))
  | RadioButtons (Expr String) (Expr (L.List String))
  | CheckBoxes (Expr (L.List Boolean)) (Expr (L.List String))
  | DropDown (Expr (L.List String)) (M.Maybe (Expr String))

instance showFormField :: Show FormField where
  show (TextBox ty def) = "(TextBox " ++ show ty ++ " " ++ show def ++ ")"
  show (RadioButtons sel ls) = "(RadioButtons " ++ show sel ++ " " ++ show ls ++ ")"
  show (CheckBoxes bs ls) = "(CheckBoxes " ++ show bs ++ " " ++ show ls ++ ")"
  show (DropDown ls def) = "(DropDown " ++ show ls ++ " " ++ show def ++ ")"

instance eqFormField :: Eq FormField where
  eq (TextBox ty1 def1) (TextBox ty2 def2) = ty1 == ty2 && def1 == def2
  eq (RadioButtons sel1 ls1) (RadioButtons sel2 ls2) = sel1 == sel2 && ls1 == ls2
  eq (CheckBoxes bs1 ls1) (CheckBoxes bs2 ls2) = bs1 == bs2 && ls1 == ls2
  eq (DropDown ls1 def1) (DropDown ls2 def2) = ls1 == ls2 && def1 == def2
  eq _ _ = false

instance arbitraryFormField :: SC.Arbitrary FormField where
  arbitrary = do
    k <- SC.chooseInt 0.0 3.0
    case k of
      0 -> TextBox <$> SC.arbitrary <*> SC.arbitrary
      1 -> RadioButtons <$> SC.arbitrary <*> genExpr (listOf SC.arbitrary)
      2 -> CheckBoxes <$> genExpr (listOf SC.arbitrary) <*> genExpr (listOf SC.arbitrary)
      _ -> DropDown <$> genExpr (listOf SC.arbitrary) <*> SC.arbitrary

listOf :: forall f a. (Monad f) => SC.GenT f a -> SC.GenT f (L.List a)
listOf = map L.toList <<< SC.arrayOf

data TextBoxType
  = PlainText
  | Numeric
  | Date
  | Time
  | DateTime

instance showTextBoxType :: Show TextBoxType where
  show PlainText = "PlainText"
  show Numeric = "Numeric"
  show Date = "Date"
  show Time = "Time"
  show DateTime = "DateTime"

instance eqTextBoxType :: Eq TextBoxType where
  eq PlainText PlainText = true
  eq Numeric Numeric = true
  eq Date Date = true
  eq Time Time = true
  eq DateTime DateTime = true
  eq _ _ = false

instance arbitraryTextBoxType :: SC.Arbitrary TextBoxType where
  arbitrary =
    SC.elements PlainText $
      L.toList
        [ PlainText
        , Numeric
        , Date
        , Time
        , DateTime
        ]

data Expr a
  = Literal a
  | Unevaluated String

instance eqExpr :: (Eq a) => Eq (Expr a) where
  eq (Literal a) (Literal b) = a == b
  eq (Unevaluated e) (Unevaluated f) = e == f
  eq _ _ = false

instance showExpr :: (Show a) => Show (Expr a) where
  show (Literal a) = "(Literal " ++ show a ++ ")"
  show (Unevaluated e) = "(Unevaluated " ++ show e ++ ")"

genExpr :: forall a. SC.Gen a -> SC.Gen (Expr a)
genExpr g = do
  b <- SC.arbitrary
  if b then Literal <$> g else Unevaluated <$> SC.arbitrary

instance arbitraryExpr :: (SC.Arbitrary a) => SC.Arbitrary (Expr a) where
  arbitrary = genExpr SC.arbitrary
