# purescript-markdown

A Purescript library for parsing SlamData's dialect of Markdown, called *SlamDown*.

## Features

In general, SlamDown is a subset of [Common Markdown](http://spec.commonmark.org/0.8/), supporting the following features:

* Leaf Blocks
  * Horizontal rules
  * ATX headers
  * Setext headers
  * Indented code blocks
  * Fenced code blocks
  * Linked reference definitions
  * Paragraphs
  * Blank line
* Container Blocks
  * Block quotes
  * List items
  * Lists
* Inlines
  * Backslash escapes
  * Entities
  * Code span
  * Emphasis and strong emphasis
  * Links
  * Images
  * Autolinks
  * Hard line breaks
  * Soft line breaks
  * Strings

Notably, HTML is not supported. Possibly, a safe subset of HTML could be added at some point in time.

The parser produces an ADT which describes the structure of the document.

## Extensions to Common Markdown

### Code Evaluation

Fenced code blocks may be evaluated by prefixing them with an exclamation point character (`!`). The result of evaluating the code is then inserted into the document at that location.

For example, in a document supporting evaluation of Javascript, the inline code block !`1 + 2` would be evaluated and the resulting number (`3`) would be inserted into the document at that location.

Code evaluation may be used for inline or block-level fenced code. 

If an info-string is specified, the evaluation must use the specified language or error. If no info-string is specified, the default language understood by the Markdown application is used.

### Form Fields

TODO

Credit to [Yevgeniy Brikman](http://brikis98.blogspot.com/2011/07/proposal-extend-markdown-syntax-to.html) for this idea.

#### Text Input

```
name = ________(default)
```

#### Radio Buttons

```
sex = (x) male () female
```

#### Checkboxes

```
phones = [] Android [x] iPhone [x] Blackberry
```

#### Dropdowns

```
city = {BOS, SFO, (NYC)}
```

#### Required Fields

```
zip code* = ________
```
