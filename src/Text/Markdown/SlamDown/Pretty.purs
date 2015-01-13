module Text.Markdown.SlamDown.Pretty (prettyPrintMd) where
    
import Data.Array (concatMap, map, (..))    

import qualified Data.String as S

import Text.Markdown.SlamDown

unlines :: [String] -> String
unlines = S.joinWith "\n"
    
prettyPrintMd :: SlamDown -> String
prettyPrintMd (SlamDown bs) = unlines $ concatMap prettyPrintBlock bs

replicateS :: Number -> String -> String
replicateS n s = S.joinWith "" (const s <$> (1 .. n))

indent :: Number -> String -> String
indent n s = replicateS n " " <> s

overLines :: (String -> String) -> [String] -> [String]
overLines f = map f <<< concatMap lines

lines :: String -> [String]
lines "" = []
lines s = S.split "\n" s

prettyPrintBlock :: Block -> [String]
prettyPrintBlock (Paragraph is) = [prettyPrintInlines is, ""]
prettyPrintBlock (Header n is) = [replicateS n "#" <> " " <> prettyPrintInlines is]
prettyPrintBlock (Blockquote bs) = overLines ((<>) "> ") (concatMap prettyPrintBlock bs)
prettyPrintBlock (List lt bss) = concatMap listItem bss 
  where
  listItem :: [Block] -> [String]
  listItem bs = 
    let ss = concatMap prettyPrintBlock bs
    in addMarker (concatMap lines ss)

  addMarker :: [String] -> [String]
  addMarker [] = []
  addMarker (s : ss) = 
    let m   = prettyPrintMarker lt
        len = S.length m
    in (m <> " " <> s) : overLines (indent (len + 1)) ss
    
  prettyPrintMarker :: ListType -> String
  prettyPrintMarker (Bullet s) = s
  prettyPrintMarker (Ordered s) = "1" <> s
prettyPrintBlock (CodeBlock Indented ss) = map (indent 4) ss
prettyPrintBlock (CodeBlock (Fenced info) ss) = ["```" <> info] <> ss <> ["```"]
prettyPrintBlock Rule = ["***"]

prettyPrintInlines :: [Inline] -> String
prettyPrintInlines is = S.joinWith "" (map prettyPrintInline is)

prettyPrintInline :: Inline -> String
prettyPrintInline (Str s) = s 
prettyPrintInline (Entity s) = s 
prettyPrintInline Space = " "
prettyPrintInline SoftBreak = "\n"  
prettyPrintInline LineBreak = "  \n"
prettyPrintInline (Emph is) = "*" <> prettyPrintInlines is <> "*"
prettyPrintInline (Strong is) = "**" <> prettyPrintInlines is <> "**"
prettyPrintInline (Code s) = "`" <> s <> "`"
prettyPrintInline (Link is url) = "[" <> prettyPrintInlines is <> "](" <> url <> ")"
prettyPrintInline (Image is url) = "![" <> prettyPrintInlines is <> "](" <> url <> ")"