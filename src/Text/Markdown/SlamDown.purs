module Text.Markdown.SlamDown where

import Prelude

import Control.Bind ((<=<))

import Data.Either (Either(..))
import Data.Foldable (foldl, mconcat, elem)
import Data.Function (on)
import Data.Identity (runIdentity)
import Data.List (List(..), concat, singleton)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (Traversable, traverse)

data SlamDown = SlamDown (List Block)

instance showSlamDown :: Show SlamDown where
  show (SlamDown bs) = "(SlamDown " ++ show bs ++ ")"

instance eqSlamDown :: Eq SlamDown where
  eq = (==) `on` show

instance ordSlamDown :: Ord SlamDown where
  compare = compare `on` show

instance semigroupSlamDown :: Semigroup SlamDown where
  append (SlamDown bs1) (SlamDown bs2) = SlamDown (bs1 <> bs2)

instance monoidSlamDown :: Monoid SlamDown where
  mempty = SlamDown mempty

data Block
  = Paragraph (List Inline)
  | Header Int (List Inline)
  | Blockquote (List Block)
  | Lst ListType (List (List Block))
  | CodeBlock CodeBlockType (List String)
  | LinkReference String String
  | Rule

instance showBlock :: Show Block where
  show (Paragraph is)        = "(Paragraph " ++ show is ++ ")"
  show (Header n is)         = "(Header " ++ show n ++ " " ++ show is ++ ")"
  show (Blockquote bs)       = "(Blockquote " ++ show bs ++ ")"
  show (Lst lt bss)         = "(List " ++ show lt ++ " " ++ show bss ++ ")"
  show (CodeBlock ca s)      = "(CodeBlock " ++ show ca ++ " " ++ show s ++ ")"
  show (LinkReference l uri) = "(LinkReference " ++ show l ++ " " ++ show uri ++ ")"
  show Rule                  = "Rule"

data Inline
  = Str String
  | Entity String
  | Space
  | SoftBreak
  | LineBreak
  | Emph (List Inline)
  | Strong (List Inline)
  | Code Boolean String
  | Link (List Inline) LinkTarget
  | Image (List Inline) String
  | FormField String Boolean FormField

instance showInline :: Show Inline where
  show (Str s)           = "(Str " ++ show s ++ ")"
  show (Entity s)        = "(Entity " ++ show s ++ ")"
  show Space             = "Space"
  show SoftBreak         = "SoftBreak"
  show LineBreak         = "LineBreak"
  show (Emph is)         = "(Emph " ++ show is ++ ")"
  show (Strong is)       = "(Strong " ++ show is ++ ")"
  show (Code e s)        = "(Code " ++ show e ++ " " ++ show s ++ ")"
  show (Link is tgt)     = "(Link " ++ show is ++ " " ++ show tgt ++ ")"
  show (Image is uri)    = "(Image " ++ show is ++ " " ++ show uri ++ ")"
  show (FormField l r f) = "(FormField " ++ show l ++ " " ++ show r ++ " " ++ show f ++ ")"

data ListType = Bullet String | Ordered String

instance showListType :: Show ListType where
  show (Bullet s)   = "(Bullet " ++ show s ++ ")"
  show (Ordered s)  = "(Ordered " ++ show s ++ ")"

instance eqListType :: Eq ListType where
  eq (Bullet s1)  (Bullet s2)  = s1 == s2
  eq (Ordered s1) (Ordered s2) = s1 == s2
  eq _            _            = false


data CodeBlockType
  = Indented
  | Fenced Boolean String

instance showCodeAttr :: Show CodeBlockType where
  show Indented      = "Indented"
  show (Fenced evaluated info) = "(Fenced " ++ show evaluated ++ " " ++ show info ++ ")"

data LinkTarget
  = InlineLink String
  | ReferenceLink (Maybe String)

instance showLinkTarget :: Show LinkTarget where
  show (InlineLink uri)    = "(InlineLink " ++ show uri ++ ")"
  show (ReferenceLink tgt) = "(ReferenceLink " ++ show tgt ++ ")"

data Expr a
  = Literal a
  | Unevaluated String

instance showExpr :: (Show a) => Show (Expr a) where
  show (Literal a)   = "(Literal " ++ show a ++ ")"
  show (Unevaluated e) = "(Unevaluated " ++ show e ++ ")"

data FormField
  = TextBox        TextBoxType (Maybe (Expr String))
  | RadioButtons   (Expr String) (Expr (List String))
  | CheckBoxes     (Expr (List Boolean)) (Expr (List String))
  | DropDown       (Expr (List String)) (Maybe (Expr String))

instance showFormField :: Show FormField where
  show (TextBox ty def) = "(TextBox " ++ show ty ++ " " ++ show def ++ ")"
  show (RadioButtons sel ls) = "(RadioButtons " ++ show sel ++ " " ++ show ls ++ ")"
  show (CheckBoxes bs ls) = "(CheckBoxes " ++ show bs ++ " " ++ show ls ++ ")"
  show (DropDown ls def) = "(DropDown " ++ show ls ++ " " ++ show def ++ ")"

data TextBoxType = PlainText | Numeric | Date | Time | DateTime

instance showTextBoxType :: Show TextBoxType where
  show PlainText = "PlainText"
  show Numeric   = "Numeric"
  show Date      = "Date"
  show Time      = "Time"
  show DateTime  = "DateTime"

instance eqTextBoxType :: Eq TextBoxType where
  eq PlainText PlainText = true
  eq Numeric Numeric = true
  eq Date Date = true
  eq Time Time = true
  eq DateTime DateTime = true
  eq _ _ = false

everywhereM :: forall m. (Monad m) => (Block -> m Block) -> (Inline -> m Inline) -> SlamDown -> m SlamDown
everywhereM b i (SlamDown bs) = SlamDown <$> traverse b' bs
  where
  b' :: Block -> m Block
  b' (Paragraph is) = (Paragraph <$> traverse i' is) >>= b
  b' (Header n is) = (Header n <$> traverse i' is) >>= b
  b' (Blockquote bs) = (Blockquote <$> traverse b' bs) >>= b
  b' (Lst lt bss) = (Lst lt <$> traverse (traverse b') bss) >>= b
  b' other = b other

  i' :: Inline -> m Inline
  i' (Emph is) = (Emph <$> traverse i' is) >>= i
  i' (Strong is) = (Strong <$> traverse i' is) >>= i
  i' (Link is uri) = (flip Link uri <$> traverse i' is) >>= i
  i' (Image is uri) = (flip Image uri <$> traverse i' is) >>= i
  i' other = i other

everywhere :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
everywhere b i = runIdentity <<< everywhereM (pure <<< b) (pure <<< i)

everywhereTopDownM :: forall m. (Monad m) => (Block -> m Block) -> (Inline -> m Inline) -> SlamDown -> m SlamDown
everywhereTopDownM b i (SlamDown bs) = SlamDown <$> traverse (b' <=< b) bs
  where
  b' :: Block -> m Block
  b' (Paragraph is) = Paragraph <$> traverse (i' <=< i) is
  b' (Header n is) = Header n <$> traverse (i' <=< i) is
  b' (Blockquote bs) = Blockquote <$> traverse (b' <=< b) bs
  b' (Lst ty bss) = Lst ty <$> traverse (traverse (b' <=< b)) bss
  b' other = b other

  i' :: Inline -> m Inline
  i' (Emph is) = Emph <$> traverse (i' <=< i) is
  i' (Strong is) = Strong <$> traverse (i' <=< i) is
  i' (Link is uri) = flip Link uri <$> traverse (i' <=< i) is
  i' (Image is uri) = flip Image uri <$> traverse (i' <=< i) is
  i' other = i other

everywhereTopDown :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
everywhereTopDown b i = runIdentity <<< everywhereTopDownM (pure <<< b) (pure <<< i)

everythingM :: forall m r. (Monad m, Monoid r) => (Block -> m r) -> (Inline -> m r) -> SlamDown -> m r
everythingM b i (SlamDown bs) = mconcat <$> traverse b' bs
  where
  b' :: Block -> m r
  b' x@(Paragraph is) = b x >>= \r -> foldl (<>) r <$> traverse i' is
  b' x@(Header _ is) = b x >>= \r -> foldl (<>) r <$> traverse i' is
  b' x@(Blockquote bs) = b x >>= \r -> foldl (<>) r <$> traverse b' bs
  b' x@(Lst _ bss) = b x >>= \r -> foldl (<>) r <<< concat <$> traverse (\bs -> traverse b' bs) bss
  b' x = b x

  i' :: Inline -> m r
  i' x@(Emph is) = i x >>= \r -> foldl (<>) r <$> traverse i' is
  i' x@(Strong is) = i x >>= \r -> foldl (<>) r <$> traverse i' is
  i' x@(Link is _) = i x >>= \r -> foldl (<>) r <$> traverse i' is
  i' x@(Image is _) = i x >>= \r -> foldl (<>) r <$> traverse i' is
  i' x = i x

everything :: forall r. (Monoid r) => (Block -> r) -> (Inline -> r) -> SlamDown -> r
everything b i = runIdentity <<< everythingM (pure <<< b) (pure <<< i)

eval :: forall m. (Monad m)
      => { code :: String -> m String
         , block :: String -> List String -> m String
         , text :: TextBoxType -> String -> m String
         , value :: String -> m String
         , list :: String -> m (List String)
         }
      -> SlamDown
      -> m SlamDown
eval fs = everywhereM b i
  where

  b :: Block -> m Block
  b (CodeBlock (Fenced true info) code) =
    CodeBlock (Fenced false info) <<< singleton <$> fs.block info code
  b other = pure $ other

  i :: Inline -> m Inline
  i (Code true code) = Code false <$> fs.code code
  i (FormField l r field) = FormField l r <$> f field
  i other = pure $ other

  f :: FormField -> m FormField
  f (TextBox ty val) = TextBox ty <$> traverse (evalExpr (fs.text ty)) val
  f (RadioButtons sel opts) = RadioButtons <$> evalExpr fs.value sel <*> evalExpr fs.list opts
  f (CheckBoxes sels vals) = do
    vals' <- evalExpr fs.list vals
    sels' <- evalExpr (\s -> fs.list s >>= pure <<< map (`elem` (getValues vals'))) sels
    pure $ CheckBoxes sels' vals'
  f (DropDown opts default) = DropDown <$> evalExpr fs.list opts <*> traverse (evalExpr fs.value) default

  evalExpr :: forall a. (String -> m a) -> Expr a -> m (Expr a)
  evalExpr _ (Literal a) = pure $ Literal a
  evalExpr e (Unevaluated s) = Literal <$> e s

  getValues :: forall a. Expr (List a) -> List a
  getValues (Literal vs) = vs
  getValues _ = Nil
