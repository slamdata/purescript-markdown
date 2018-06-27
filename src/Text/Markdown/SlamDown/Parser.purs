module Text.Markdown.SlamDown.Parser
  ( parseMd
  , validateBlock
  , validateSlamDown
  ) where

import Prelude hiding (min)

import Data.Either (Either)
import Data.Foldable (any, all)
import Data.List ((:))
import Data.List as L
import Data.Maybe as M
import Data.String (Pattern(..), Replacement(..), drop, length, replace, split, take, trim) as S
import Data.String.CodeUnits (countPrefix, dropWhile, singleton) as S
import Data.String.Regex as RGX
import Data.String.Regex.Unsafe as URX
import Data.String.Regex.Flags as RXF
import Data.Traversable (traverse)
import Data.Validation.Semigroup as V

import Partial.Unsafe (unsafePartial)

import Text.Markdown.SlamDown.Parser.Inline as Inline
import Text.Markdown.SlamDown.Parser.References as Ref
import Text.Markdown.SlamDown.Syntax as SD

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
allChars p = all p <<< S.split (S.Pattern "")

removeNonIndentingSpaces ∷ String → String
removeNonIndentingSpaces s
  | S.countPrefix (isSpace <<< S.singleton) s < 4 = S.dropWhile (isSpace <<< S.singleton) s
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
    level = S.countPrefix (\c → S.singleton c == "#") s
    rest = S.drop level s
  in
    level >= 1 && level <= 6 && S.take 1 rest == " "

splitATXHeader ∷ String → { level ∷ Int, contents ∷ String }
splitATXHeader s =
  let
    level = S.countPrefix (\c → S.singleton c == "#") s
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
countLeadingSpaces = S.countPrefix (isSpace <<< S.singleton)

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
    n = S.countPrefix (isDigit <<< S.singleton) s
    next = S.take 1 (S.drop n s)
    ls = countLeadingSpaces (S.drop (n + 1) s)
  in
    n > 0 && (next == "." || next == ")") && ls > 0

listItemType ∷ String → SD.ListType
listItemType s
  | isBulleted s = SD.Bullet (S.take 1 s)
  | otherwise =
      let n = S.countPrefix (isDigit <<< S.singleton) s
      in SD.Ordered (S.take 1 (S.drop n s))

listItemIndent ∷ String → Int
listItemIndent s
  | isBulleted s = 1 + min 4 (countLeadingSpaces (S.drop 1 s))
  | otherwise =
      let n = S.countPrefix (isDigit <<< S.singleton) s
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
  isSimpleFence s' = S.countPrefix (isFenceChar <<< S.singleton) s' >= 3

isEvaluatedCode ∷ String → Boolean
isEvaluatedCode s = S.take 1 s == "!"

isFenceChar ∷ String → Boolean
isFenceChar "~" = true
isFenceChar "`" = true
isFenceChar _ = false

codeFenceInfo ∷ String → String
codeFenceInfo = S.trim <<< S.dropWhile (isFenceChar <<< S.singleton)

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
  isClosingFence s = S.countPrefix (\c → S.singleton c == fence) (removeNonIndentingSpaces s) >= 3

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
        b = unsafePartial M.fromJust $ Ref.parseLinkReference s1
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
  → Either String (L.List (SD.Block a))
parseBlocks =
  case _ of
    L.Nil → pure L.Nil
    (CText s) : (CSetextHeader n) : cs → do
      hd ← Inline.parseInlines $ L.singleton s
      tl ← parseBlocks cs
      pure $ (SD.Header n hd) : tl
    (CText s) : cs → do
      let
        sp = L.span isTextContainer cs
      is ← Inline.parseInlines $ s : (map getCText sp.init)
      tl ← parseBlocks sp.rest
      pure $ (SD.Paragraph is) : tl
    CRule : cs →
      map (SD.Rule : _) $ parseBlocks cs
    (CATXHeader n s) : cs → do
      hd ← Inline.parseInlines $ L.singleton s
      tl ← parseBlocks cs
      pure $ (SD.Header n hd) : tl
    (CBlockquote cs) : cs1 → do
      hd ← parseBlocks cs
      tl ← parseBlocks cs1
      pure $ (SD.Blockquote hd) : tl
    (CListItem lt cs) : cs1 → do
      let
        sp = L.span (isListItem lt) cs1
      bs ← parseBlocks cs
      bss ← traverse (parseBlocks <<< getCListItem) sp.init
      tl ← parseBlocks sp.rest
      pure $ (SD.Lst lt (bs : bss)) : tl
    (CCodeBlockIndented ss) : cs →
      map ((SD.CodeBlock SD.Indented ss) : _) $ parseBlocks cs
    (CCodeBlockFenced eval info ss) : cs →
      map ((SD.CodeBlock (SD.Fenced eval info) ss) : _) $ parseBlocks cs
    (CLinkReference b) : cs →
      map (b : _) $ parseBlocks cs
    L.Cons _ cs →
      parseBlocks cs

validateBlock ∷ ∀ a. SD.Block a → V.V (Array String) (SD.Block a)
validateBlock =
  case _ of
    SD.Paragraph inls → SD.Paragraph <$> traverse Inline.validateInline inls
    SD.Header i inls → SD.Header i <$> traverse Inline.validateInline inls
    SD.Blockquote bls → SD.Blockquote <$> traverse validateBlock bls
    SD.Lst lt blss → SD.Lst lt <$> traverse (traverse validateBlock) blss
    b → pure b

validateSlamDown ∷ ∀ a. SD.SlamDownP a → V.V (Array String) (SD.SlamDownP a)
validateSlamDown (SD.SlamDown bls) = SD.SlamDown <$> traverse validateBlock bls

tabsToSpaces ∷ String → String
tabsToSpaces = S.replace (S.Pattern "\t") (S.Replacement "    ")

parseMd ∷ ∀ a. (SD.Value a) ⇒ String → Either String (SD.SlamDownP a)
parseMd s = map SD.SlamDown bs
  where
    slashR = URX.unsafeRegex "\\r" RXF.global
    lines = L.fromFoldable $ S.split (S.Pattern "\n") $ RGX.replace slashR "" $ tabsToSpaces s
    ctrs = parseContainers mempty lines
    bs = parseBlocks ctrs
