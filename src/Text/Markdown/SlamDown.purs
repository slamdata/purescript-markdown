module Text.Markdown.SlamDown where

import Data.Array (map)
import Data.Maybe
import Data.Monoid
import Data.Function (on)

data SlamDown = SlamDown [Block]

instance showSlamDown :: Show SlamDown where
  show (SlamDown bs) = "(SlamDown " ++ show bs ++ ")"
  
instance eqSlamDown :: Eq SlamDown where
  (==) = (==) `on` show
  (/=) = (/=) `on` show
  
instance ordSlamDown :: Ord SlamDown where
  compare = compare `on` show  
  
instance semigroupSlamDown :: Semigroup SlamDown where
  (<>) (SlamDown bs1) (SlamDown bs2) = SlamDown (bs1 <> bs2)
  
instance monoidSlamDown :: Monoid SlamDown where
  mempty = SlamDown []

data Block
  = Paragraph [Inline]
  | Header Number [Inline]   
  | Blockquote [Block]  
  | List ListType [[Block]]  
  | CodeBlock CodeBlockType [String] 
  | Rule

instance showBlock :: Show Block where
  show (Paragraph is)   = "(Paragraph " ++ show is ++ ")"
  show (Header n is)    = "(Header " ++ show n ++ " " ++ show is ++ ")"
  show (Blockquote bs)  = "(Blockquote " ++ show bs ++ ")"
  show (List lt bss)    = "(List " ++ show lt ++ " " ++ show bss ++ ")"
  show (CodeBlock ca s) = "(CodeBlock " ++ show ca ++ " " ++ show s ++ ")"
  show Rule             = "Rule"

data Inline
  = Str String 
  | Entity String 
  | Space
  | SoftBreak  
  | LineBreak  
  | Emph [Inline]  
  | Strong [Inline] 
  | Code Boolean String
  | Link [Inline] String   
  | Image [Inline] String
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
  show (Link is uri)     = "(Link " ++ show is ++ " " ++ show uri ++ ")"
  show (Image is uri)    = "(Image " ++ show is ++ " " ++ show uri ++ ")"
  show (FormField l r f) = "(FormField " ++ show l ++ " " ++ show r ++ " " ++ show f ++ ")"
   
data ListType = Bullet String | Ordered String

instance showListType :: Show ListType where
  show (Bullet s)   = "(Bullet " ++ show s ++ ")"
  show (Ordered s)  = "(Ordered " ++ show s ++ ")"

instance eqListType :: Eq ListType where
  (==) (Bullet s1)  (Bullet s2)  = s1 == s2
  (==) (Ordered s1) (Ordered s2) = s1 == s2
  (==) _            _            = false
  (/=) x            y            = not (x == y)

data CodeBlockType 
  = Indented
  | Fenced Boolean String

instance showCodeAttr :: Show CodeBlockType where
  show Indented      = "Indented"
  show (Fenced eval info) = "(Fenced " ++ show eval ++ " " ++ show info ++ ")"
 
data Expr a
  = Literal a
  | Evaluated String 

instance showExpr :: (Show a) => Show (Expr a) where
  show (Literal a)   = "(Literal " ++ show a ++ ")"
  show (Evaluated s) = "(Evaluated " ++ show s ++ ")"
 
data FormField
  = TextBox        TextBoxType (Expr String)
  | RadioButtons   (Expr String) (Expr [String])
  | CheckBoxes     (Expr [Boolean]) (Expr [String])
  | DropDown       (Expr [String]) (Expr String)
  
instance showFormField :: Show FormField where
  show (TextBox ty def) = "(TextBox " ++ show ty ++ " " ++ show def ++ ")"
  show (RadioButtons sel ls) = "(RadioButtons " ++ show sel ++ " " ++ show ls ++ ")"
  show (CheckBoxes bs ls) = "(CheckBoxes " ++ show bs ++ " " ++ show ls ++ ")"
  show (DropDown ls def) = "(DropDown " ++ show ls ++ " " ++ show def ++ ")"
 
data TextBoxType = PlainText | Date | Time | DateTime

instance showTextBoxType :: Show TextBoxType where
  show PlainText = "PlainText" 
  show Date      = "Date" 
  show Time      = "Time" 
  show DateTime  = "DateTime" 
 
everywhere :: (Block -> Block) -> (Inline -> Inline) -> SlamDown -> SlamDown
everywhere b i (SlamDown bs) = SlamDown (map b' bs)
  where
  b' :: Block -> Block
  b' (Paragraph is) = b (Paragraph (map i' is))
  b' (Header n is) = b (Header n (map i' is))
  b' (Blockquote bs) = b (Blockquote (map b' bs))
  b' (List lt bss) = b (List lt (map (map b') bss))
  b' other = b other
  
  i' :: Inline -> Inline
  i' (Emph is)        = i (Emph (map i' is))
  i' (Strong is)      = i (Strong (map i' is))
  i' (Link is uri)    = i (Link (map i' is) uri)
  i' (Image is uri)   = i (Image (map i' is) uri)
  i' other = i other

eval :: (Maybe String -> [String] -> String) -> SlamDown -> SlamDown
eval f = everywhere b i
  where
  b :: Block -> Block
  b (CodeBlock (Fenced true info) code) = CodeBlock (Fenced false info) [f (Just info) code]
  b other = other

  i :: Inline -> Inline
  i (Code true code) = Code false (f Nothing [code])
  i other = other