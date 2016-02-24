module Text.Markdown.SlamDown.Parser
  ( parseMd
  , validateBlock
  , validateSlamDown
  ) where

import Prelude

import Data.Foldable (any, all)
import Data.List as L
import Data.Maybe as M
import Data.Monoid (mempty)
import Data.String as S
import Data.Traversable (traverse)
import Data.Validation as V

import Text.Markdown.SlamDown.Syntax as SD
import Text.Markdown.SlamDown.Parser.Inline as Inline
import Text.Markdown.SlamDown.Parser.References as Ref

data Container a
  = CText String
  | CBlank
  | CRule
  | CATXHeader Int String
  | CSetextHeader Int
  | CBlockquote (L.List (Container a))
  | CListItem SD.ListType (L.List (Container a))
  | CCodeBlockFenced Boolean String (L.List String)
  | CCodeBlockIndented (L.List String)
  | CLinkReference (SD.Block a)

isSpace ∷ String → Boolean
isSpace " " = true
isSpace _  = false

isDigit ∷ String → Boolean
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
isDigit _ = false

allChars ∷ (String → Boolean) → String → Boolean
allChars p = all p <<< S.split ""

removeNonIndentingSpaces ∷ String → String
removeNonIndentingSpaces s
  | S.count (isSpace <<< S.fromChar) s < 4 = S.dropWhile (isSpace <<< S.fromChar) s
  | otherwise = s

isRuleChar ∷ String → Boolean
isRuleChar "*" = true
isRuleChar "-" = true
isRuleChar "_" = true
isRuleChar _   = false

isRule ∷ String → Boolean
isRule s =
  allChars isRuleChar s
    && S.length s >= 3
    && allChars ((==) (S.take 1 s)) s

isATXHeader ∷ String → Boolean
isATXHeader s =
  let
    level = S.count (\c → S.fromChar c == "#") s
    rest = S.drop level s
  in
    level >= 1 && level <= 6 && S.take 1 rest == " "

splitATXHeader ∷ String → { level ∷ Int, contents ∷ String }
splitATXHeader s =
  let
    level = S.count (\c → S.fromChar c == "#") s
    contents = S.drop (level + 1) s
  in
    { level: level
    , contents: contents
    }

-- Takes the last parsed container as an argument
-- to avoid parsing a rule as a header
isSetextHeader ∷ ∀ a. String → M.Maybe (Container a) → Boolean
isSetextHeader s (M.Just (CText _)) = S.length s >= 1 && any (\c → allChars ((==) c) s) ["=", "-"]
isSetextHeader _ _ = false

setextLevel ∷ String → Int
setextLevel s
  | S.take 1 s == "=" = 1
  | otherwise         = 2

isBlockquoteLine ∷ String → Boolean
isBlockquoteLine s = S.take 1 (removeNonIndentingSpaces s) == ">"

splitBlockquote ∷ L.List String → { blockquoteLines ∷ L.List String , otherLines ∷ L.List String }
splitBlockquote ss =
  let
    sp = L.span isBlockquoteLine ss
    bq = map (blockquoteContents <<< removeNonIndentingSpaces) sp.init
  in
    { blockquoteLines: bq
    , otherLines: sp.rest
    }
  where
  blockquoteContents ∷ String → String
  blockquoteContents s = S.drop (if S.take 2 s == "> " then 2 else 1) s

countLeadingSpaces ∷ String → Int
countLeadingSpaces = S.count (isSpace <<< S.fromChar)

isBulleted ∷ String → Boolean
isBulleted s =
  let
    b  = S.take 1 s
    ls = countLeadingSpaces (S.drop 1 s)
  in
    isBullet b && ls > 0 && ls < 5
  where
  isBullet ∷ String → Boolean
  isBullet "*" = true
  isBullet "+" = true
  isBullet "-" = true
  isBullet _   = false

isOrderedListMarker ∷ String → Boolean
isOrderedListMarker s =
  let
    n = S.count (isDigit <<< S.fromChar) s
    next = S.take 1 (S.drop n s)
    ls = countLeadingSpaces (S.drop (n + 1) s)
  in
    n > 0 && (next == "." || next == ")") && ls > 0

listItemType ∷ String → SD.ListType
listItemType s
  | isBulleted s = SD.Bullet (S.take 1 s)
  | otherwise =
      let n = S.count (isDigit <<< S.fromChar) s
      in SD.Ordered (S.take 1 (S.drop n s))

listItemIndent ∷ String → Int
listItemIndent s
  | isBulleted s = 1 + min 4 (countLeadingSpaces (S.drop 1 s))
  | otherwise =
      let n = S.count (isDigit <<< S.fromChar) s
      in n + 1 + min 4 (countLeadingSpaces (S.drop (n + 1) s))

isListItemLine ∷ String → Boolean
isListItemLine s =
  let s' = removeNonIndentingSpaces s
  in isBulleted s' || isOrderedListMarker s'

isIndentedTo ∷ Int → String → Boolean
isIndentedTo n s = countLeadingSpaces s >= n

splitListItem
  ∷ String
  → L.List String
  → { listType ∷ SD.ListType
     , listItemLines ∷ L.List String
     , otherLines ∷ L.List String
     }
splitListItem s ss =
  let
    s1 = removeNonIndentingSpaces s
    sp = L.span (isIndentedTo indent) ss
    indent = listItemIndent s1
    listItemLines = L.Cons (S.drop indent s1) $ map (S.drop indent) sp.init
    listType = listItemType s1
  in
    { listType: listType
    , listItemLines: listItemLines
    , otherLines: sp.rest
    }

isIndentedChunk ∷ String → Boolean
isIndentedChunk s = isIndentedTo 4 s

fromIndentedChunk ∷ String → String
fromIndentedChunk = S.drop 4

splitIndentedChunks
  ∷ L.List String
  → { codeLines ∷ L.List String
    , otherLines ∷ L.List String
    }
splitIndentedChunks ss =
  let
    sp = L.span isIndentedChunk ss
    codeLines = map fromIndentedChunk sp.init
  in
    { codeLines: codeLines
    , otherLines: sp.rest
    }

isCodeFence ∷ String → Boolean
isCodeFence s = isSimpleFence s || (isEvaluatedCode s && isSimpleFence (S.drop 1 s))
  where
  isSimpleFence s = S.count (isFenceChar <<< S.fromChar) s >= 3

isEvaluatedCode ∷ String → Boolean
isEvaluatedCode s = S.take 1 s == "!"

isFenceChar ∷ String → Boolean
isFenceChar "~" = true
isFenceChar "`" = true
isFenceChar _ = false

codeFenceInfo ∷ String → String
codeFenceInfo = S.trim <<< S.dropWhile (isFenceChar <<< S.fromChar)

codeFenceChar ∷ String → String
codeFenceChar = S.take 1

splitCodeFence
  ∷ Int
  → String
  → L.List String
  → { codeLines ∷ L.List String
     , otherLines ∷ L.List String
     }
splitCodeFence indent fence ss =
  let
    sp = L.span (not <<< isClosingFence) ss
    codeLines = map removeIndentTo sp.init
  in
    { codeLines: codeLines
    , otherLines: L.drop 1 sp.rest
    }
  where
  isClosingFence ∷ String → Boolean
  isClosingFence s = S.count (\c → S.fromChar c == fence) (removeNonIndentingSpaces s) >= 3

  removeIndentTo ∷ String → String
  removeIndentTo s = S.drop (min indent (countLeadingSpaces s)) s

isLinkReference ∷ String → Boolean
isLinkReference s = S.take 1 s == "[" && M.isJust (Ref.parseLinkReference s)

min ∷ ∀ a. (Ord a) ⇒ a → a → a
min n m = if n < m then n else m

parseContainers
  ∷ ∀ a
  . L.List (Container a)
  → L.List String
  → L.List (Container a)
parseContainers acc L.Nil = L.reverse acc
parseContainers acc (L.Cons s ss)
  | allChars isSpace s =
      parseContainers (L.Cons CBlank acc) ss
  | isATXHeader (removeNonIndentingSpaces s) =
      let o = splitATXHeader (removeNonIndentingSpaces s)
      in parseContainers (L.Cons (CATXHeader o.level o.contents) acc) ss
  | isSetextHeader (removeNonIndentingSpaces (S.trim s)) (L.last acc) =
      parseContainers (L.Cons (CSetextHeader $ setextLevel (removeNonIndentingSpaces (S.trim s))) acc) ss
  | isRule (removeNonIndentingSpaces s) =
      parseContainers (L.Cons CRule acc) ss
  | isBlockquoteLine s =
      let o = splitBlockquote $ L.Cons s ss
      in parseContainers (L.Cons (CBlockquote (parseContainers mempty o.blockquoteLines)) acc) o.otherLines
  | isListItemLine s =
      let o = splitListItem s ss
      in parseContainers (L.Cons (CListItem o.listType $ parseContainers mempty o.listItemLines) acc) o.otherLines
  | isIndentedChunk s =
      let o = splitIndentedChunks (L.Cons s ss)
      in parseContainers (L.Cons (CCodeBlockIndented o.codeLines) acc) o.otherLines
  | isCodeFence (removeNonIndentingSpaces s) =
      let
        s1 = removeNonIndentingSpaces s
        eval = isEvaluatedCode s1
        s2 = if eval then S.drop 1 s1 else s1
        info = codeFenceInfo s2
        ch = codeFenceChar s2
        o = splitCodeFence (countLeadingSpaces s) ch ss
      in
        parseContainers (L.Cons (CCodeBlockFenced eval info o.codeLines) acc) o.otherLines
  | isLinkReference (removeNonIndentingSpaces s) =
      let
        s1 = removeNonIndentingSpaces s
        b = Data.Maybe.Unsafe.fromJust $ Ref.parseLinkReference s1
      in
        parseContainers (L.Cons (CLinkReference b) acc) ss
  | otherwise = parseContainers (L.Cons (CText s) acc) ss

isTextContainer ∷ ∀ a. Container a → Boolean
isTextContainer (CText _) = true
isTextContainer _ = false

getCText ∷ ∀ a. Container a → String
getCText (CText s) = s
getCText _ = ""

isListItem ∷ ∀ a. SD.ListType → Container a → Boolean
isListItem lt1 (CListItem lt2 _) = lt1 == lt2
isListItem _ _ = false

getCListItem ∷ ∀ a. Container a → L.List (Container a)
getCListItem (CListItem _ cs) = cs
getCListItem _ = L.Nil

parseBlocks
  ∷ ∀ a
  . (SD.Value a)
  ⇒ L.List (Container a)
  → L.List (SD.Block a)
parseBlocks cs =
  case cs of
    L.Nil → L.Nil
    L.Cons (CText s) (L.Cons (CSetextHeader n) cs) →
      L.Cons (SD.Header n (Inline.parseInlines $ L.singleton s)) $ parseBlocks cs
    L.Cons (CText s) cs →
      let
        sp = L.span isTextContainer cs
        is = Inline.parseInlines $ L.Cons s (map getCText sp.init)
      in
        L.Cons (SD.Paragraph is) $ parseBlocks sp.rest
    L.Cons CRule cs →
      L.Cons SD.Rule $ parseBlocks cs
    L.Cons (CATXHeader n s) cs →
      L.Cons (SD.Header n (Inline.parseInlines $ L.singleton s)) $ parseBlocks cs
    L.Cons (CBlockquote cs) cs1 →
      L.Cons (SD.Blockquote $ parseBlocks cs) $ parseBlocks cs1
    L.Cons (CListItem lt cs) cs1 →
      let
        sp = L.span (isListItem lt) cs1
        bs = parseBlocks cs
        bss = map (parseBlocks <<< getCListItem) sp.init
      in
        L.Cons (SD.Lst lt (L.Cons bs bss)) $ parseBlocks sp.rest
    L.Cons (CCodeBlockIndented ss) cs →
      L.Cons (SD.CodeBlock SD.Indented ss) $ parseBlocks cs
    L.Cons (CCodeBlockFenced eval info ss) cs →
      L.Cons (SD.CodeBlock (SD.Fenced eval info) ss) $ parseBlocks cs
    L.Cons (CLinkReference b) cs →
      L.Cons b $ parseBlocks cs
    L.Cons _ cs →
      parseBlocks cs

validateBlock ∷ ∀ a. SD.Block a → V.V (Array String) (SD.Block a)
validateBlock b =
  case b of
    SD.Paragraph inls → SD.Paragraph <$> traverse Inline.validateInline inls
    SD.Header i inls → SD.Header i <$> traverse Inline.validateInline inls
    SD.Blockquote bls → SD.Blockquote <$> traverse validateBlock bls
    SD.Lst lt blss → SD.Lst lt <$> traverse (traverse validateBlock) blss
    _ → pure b

validateSlamDown ∷ ∀ a. SD.SlamDownP a → V.V (Array String) (SD.SlamDownP a)
validateSlamDown (SD.SlamDown bls) = SD.SlamDown <$> traverse validateBlock bls

tabsToSpaces ∷ String → String
tabsToSpaces = S.replace "\t" "    "

parseMd ∷ ∀ a. (SD.Value a) ⇒ String → SD.SlamDownP a
parseMd s = SD.SlamDown bs
  where
    lines = L.toList $ S.split "\n" $ S.replace "\r" "" $ tabsToSpaces s
    ctrs = parseContainers mempty lines
    bs = parseBlocks ctrs
