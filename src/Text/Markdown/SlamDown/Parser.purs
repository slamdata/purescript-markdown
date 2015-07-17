module Text.Markdown.SlamDown.Parser (parseMd) where

import Prelude
import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Foldable (any, all)
import Data.List (List(..), toList, reverse, last, span, drop, singleton)
import Data.Monoid (mempty)

import qualified Data.Char as S
import qualified Data.String as S
    
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser.Utils
import Text.Markdown.SlamDown.Parser.Inline
import Text.Markdown.SlamDown.Parser.References

data Container 
  = CText String
  | CBlank
  | CRule
  | CATXHeader Int String
  | CSetextHeader Int
  | CBlockquote (List Container)
  | CListItem ListType (List Container)
  | CCodeBlockFenced Boolean String (List String)
  | CCodeBlockIndented (List String)
  | CLinkReference Block

isSpace :: String -> Boolean
isSpace " "  = true
isSpace _    = false

isDigit :: String -> Boolean
isDigit "0" = true
isDigit "1" = true
isDigit "2" = true
isDigit "3" = true
isDigit "4" = true
isDigit "5" = true
isDigit "6" = true
isDigit "7" = true
isDigit "8" = true
isDigit "9" = true
isDigit _   = false

allChars :: (String -> Boolean) -> String -> Boolean
allChars p = all p <<< S.split ""

removeNonIndentingSpaces :: String -> String
removeNonIndentingSpaces s 
  | S.count (isSpace <<< S.fromChar) s < 4 = S.dropWhile (isSpace <<< S.fromChar) s
  | otherwise = s

isRuleChar :: String -> Boolean
isRuleChar "*" = true
isRuleChar "-" = true
isRuleChar "_" = true
isRuleChar _   = false

isRule :: String -> Boolean
isRule s = allChars isRuleChar s &&
           S.length s >= 3 &&
           allChars ((==) (S.take 1 s)) s

isATXHeader :: String -> Boolean
isATXHeader s = 
  let level = S.count (\c -> S.fromChar c == "#") s
      rest  = S.drop level s
  in level >= 1 && level <= 6 && S.take 1 rest == " "

splitATXHeader :: String -> { level :: Int, contents :: String }
splitATXHeader s = 
  let level    = S.count (\c -> S.fromChar c == "#") s
      contents = S.drop (level + 1) s
  in { level: level, contents: contents } 

-- Takes the last parsed container as an argument
-- to avoid parsing a rule as a header
isSetextHeader :: String -> Maybe Container -> Boolean
isSetextHeader s (Just (CText _)) = S.length s >= 1 && any (\c -> allChars ((==) c) s) ["=", "-"]
isSetextHeader _ _ = false

setextLevel :: String -> Int
setextLevel s | S.take 1 s == "=" = 1
              | otherwise         = 2

isBlockquoteLine :: String -> Boolean
isBlockquoteLine s = S.take 1 (removeNonIndentingSpaces s) == ">"

splitBlockquote :: List String -> { blockquoteLines :: List String
                                  , otherLines :: List String }
splitBlockquote ss =
  let sp = span isBlockquoteLine ss
      bq = map (blockquoteContents <<< removeNonIndentingSpaces) sp.init
  in { blockquoteLines: bq, otherLines: sp.rest }
  where
  blockquoteContents :: String -> String
  blockquoteContents s = S.drop (if S.take 2 s == "> " then 2 else 1) s

countLeadingSpaces :: String -> Int
countLeadingSpaces = S.count (isSpace <<< S.fromChar)

isBulleted :: String -> Boolean
isBulleted s = 
  let b  = S.take 1 s
      ls = countLeadingSpaces (S.drop 1 s)
  in isBullet b && ls > 0 && ls < 5
  where
  isBullet "*" = true
  isBullet "+" = true
  isBullet "-" = true
  isBullet _   = false

isOrderedListMarker :: String -> Boolean
isOrderedListMarker s =
  let n    = S.count (isDigit <<< S.fromChar) s
      next = S.take 1 (S.drop n s)
      ls   = countLeadingSpaces (S.drop (n + 1) s) 
  in n > 0 && (next == "." || next == ")") && ls > 0

listItemType :: String -> ListType
listItemType s
  | isBulleted s = Bullet (S.take 1 s)
  | otherwise = let n = S.count (isDigit <<< S.fromChar) s
                in Ordered (S.take 1 (S.drop n s))

listItemIndent :: String -> Int
listItemIndent s 
  | isBulleted s = 1 + min 4 (countLeadingSpaces (S.drop 1 s))
  | otherwise = let n = S.count (isDigit <<< S.fromChar) s
                in n + 1 + min 4 (countLeadingSpaces (S.drop (n + 1) s))

isListItemLine :: String -> Boolean
isListItemLine s = 
  let s' = removeNonIndentingSpaces s
  in isBulleted s' || isOrderedListMarker s'

isIndentedTo :: Int -> String -> Boolean
isIndentedTo n s = countLeadingSpaces s >= n

splitListItem :: List String -> { listType :: ListType
                                , listItemLines :: List String
                                , otherLines :: List String }
splitListItem (Cons s ss) =
  let s1 = removeNonIndentingSpaces s
      sp = span (isIndentedTo indent) ss
      indent = listItemIndent s1
      listItemLines = Cons (S.drop indent s1) $ map (S.drop indent) sp.init
      listType = listItemType s1
  in { listType: listType
     , listItemLines: listItemLines
     , otherLines: sp.rest 
     }

isIndentedChunk :: String -> Boolean
isIndentedChunk s = isIndentedTo 4 s

fromIndentedChunk :: String -> String
fromIndentedChunk = S.drop 4

splitIndentedChunks :: List String -> { codeLines :: List String
                                      , otherLines :: List String }
splitIndentedChunks ss =
  let sp        = span isIndentedChunk ss
      codeLines = map fromIndentedChunk sp.init
  in { codeLines: codeLines, otherLines: sp.rest }

isCodeFence :: String -> Boolean
isCodeFence s = isSimpleFence s || (isEvaluatedCode s && isSimpleFence (S.drop 1 s))
  where
  isSimpleFence s = S.count (isFenceChar <<< S.fromChar) s >= 3

isEvaluatedCode :: String -> Boolean
isEvaluatedCode s = S.take 1 s == "!" 

isFenceChar :: String -> Boolean
isFenceChar "~" = true
isFenceChar "`" = true
isFenceChar _   = false

codeFenceInfo :: String -> String
codeFenceInfo = trimEnd <<< trim <<< S.dropWhile (isFenceChar <<< S.fromChar)

codeFenceChar :: String -> String
codeFenceChar = S.take 1

splitCodeFence :: Int -> String -> List String ->
                  { codeLines :: List String
                  , otherLines :: List String }
splitCodeFence indent fence ss =
  let sp        = span (not <<< isClosingFence) ss
      codeLines = map removeIndentTo sp.init
  in { codeLines: codeLines, otherLines: drop 1 sp.rest }
  where
  isClosingFence :: String -> Boolean 
  isClosingFence s = S.count (\c -> S.fromChar c == fence) (removeNonIndentingSpaces s) >= 3 

  removeIndentTo :: String -> String
  removeIndentTo s = S.drop (min indent (countLeadingSpaces s)) s

isLinkReference :: String -> Boolean
isLinkReference s = S.take 1 s == "[" && isJust (parseLinkReference s)

min :: forall a. (Ord a) => a -> a -> a
min n m = if n < m then n else m

parseContainers :: List Container -> List String -> List Container
parseContainers acc Nil = reverse acc
parseContainers acc (Cons s ss)
  | allChars isSpace s = 
      parseContainers (Cons CBlank acc) ss
  | isATXHeader (removeNonIndentingSpaces s) = 
      let o = splitATXHeader (removeNonIndentingSpaces s)
      in parseContainers (Cons (CATXHeader o.level o.contents) acc) ss
  | isSetextHeader (removeNonIndentingSpaces (trimEnd s)) (last acc) =
      parseContainers (Cons (CSetextHeader $ setextLevel (removeNonIndentingSpaces (trimEnd s))) acc) ss
  | isRule (removeNonIndentingSpaces s) = 
      parseContainers (Cons CRule acc) ss
  | isBlockquoteLine s =
      let o = splitBlockquote $ Cons s ss
      in parseContainers (Cons (CBlockquote (parseContainers mempty o.blockquoteLines)) acc) o.otherLines 
  | isListItemLine s =
      let o = splitListItem (Cons s ss)
      in parseContainers (Cons (CListItem o.listType $ parseContainers mempty o.listItemLines) acc) o.otherLines
  | isIndentedChunk s =
      let o = splitIndentedChunks (Cons s ss)
      in parseContainers (Cons (CCodeBlockIndented o.codeLines) acc) o.otherLines
  | isCodeFence (removeNonIndentingSpaces s) =
      let s1   = removeNonIndentingSpaces s
          eval = isEvaluatedCode s1
          s2   = if eval then S.drop 1 s1 else s1
          info = codeFenceInfo s2
          ch   = codeFenceChar s2
          o    = splitCodeFence (countLeadingSpaces s) ch ss
      in parseContainers (Cons (CCodeBlockFenced eval info o.codeLines) acc) o.otherLines
  | isLinkReference (removeNonIndentingSpaces s) =
      let s1 = removeNonIndentingSpaces s
          b  = fromJust $ parseLinkReference s1
      in parseContainers (Cons (CLinkReference b) acc) ss
  | otherwise = parseContainers (Cons (CText s) acc) ss 

isTextContainer :: Container -> Boolean
isTextContainer (CText _) = true
isTextContainer _ = false

getCText :: Container -> String
getCText (CText s) = s

isListItem :: ListType -> Container -> Boolean
isListItem lt1 (CListItem lt2 _) = lt1 == lt2
isListItem _ _ = false

getCListItem :: Container -> List Container
getCListItem (CListItem _ cs) = cs

parseBlocks :: List Container -> List Block
parseBlocks Nil = Nil
parseBlocks (Cons (CText s) (Cons (CSetextHeader n) cs)) =
  Cons (Header n (parseInlines $ singleton s)) $ parseBlocks cs
parseBlocks (Cons (CText s) cs) =
  let sp = span isTextContainer cs
      is = parseInlines (Cons s (map getCText sp.init))
  in Cons (Paragraph is) $ parseBlocks sp.rest
parseBlocks (Cons CRule cs) =
  Cons Rule $ parseBlocks cs
parseBlocks (Cons (CATXHeader n s) cs) =
  Cons (Header n (parseInlines $ singleton s)) $ parseBlocks cs 
parseBlocks (Cons (CBlockquote cs) cs1) =
  Cons (Blockquote $ parseBlocks cs) $ parseBlocks cs1
parseBlocks (Cons (CListItem lt cs) cs1) =
  let sp = span (isListItem lt) cs1
      bs = parseBlocks cs
      bss = map (parseBlocks <<< getCListItem) sp.init
  in Cons (Lst lt (Cons bs bss)) $ parseBlocks sp.rest
parseBlocks (Cons (CCodeBlockIndented ss) cs) =
  Cons (CodeBlock Indented ss) $ parseBlocks cs
parseBlocks (Cons (CCodeBlockFenced eval info ss) cs) =
  Cons (CodeBlock (Fenced eval info) ss) $ parseBlocks cs
parseBlocks (Cons (CLinkReference b) cs) =
  Cons b $ parseBlocks cs
parseBlocks (Cons _ cs) =
  parseBlocks cs

tabsToSpaces :: String -> String
tabsToSpaces = S.replace "\t" "    "

parseMd :: String -> SlamDown
parseMd s = 
  let lines = toList $ S.split "\n" $ S.replace "\r" "" $ tabsToSpaces s
      ctrs  = parseContainers mempty lines
      bs    = parseBlocks ctrs
  in SlamDown bs
