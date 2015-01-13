module Text.Markdown.SlamDown where

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
  | Code String
  | Link [Inline] String   
  | Image [Inline] String

instance showInline :: Show Inline where
  show (Str s)          = "(Str " ++ show s ++ ")"
  show (Entity s)       = "(Entity " ++ show s ++ ")"
  show Space            = "Space"
  show SoftBreak        = "SoftBreak"
  show LineBreak        = "LineBreak"
  show (Emph is)        = "(Emph " ++ show is ++ ")"
  show (Strong is)      = "(Strong " ++ show is ++ ")"
  show (Code s)         = "(Code " ++ show s ++ ")"
  show (Link is uri)    = "(Link " ++ show is ++ " " ++ show uri ++ ")"
  show (Image is uri)   = "(Image " ++ show is ++ " " ++ show uri ++ ")"
   
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
  | Fenced String

instance showCodeAttr :: Show CodeBlockType where
  show Indented      = "Indented"
  show (Fenced info) = "(Fenced " ++ show info ++ ")"
 
