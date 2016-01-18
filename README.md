# purescript-markdown

A Purescript library for parsing SlamData's dialect of Markdown, called *SlamDown*, which is mostly a safe, clean subset of CommonMark.

## Usage


```purescript
import Text.Markdown.SlamDown
import Text.Markdown.SlamDown.Parser

-- parsing
case parseMd "# foo" of 
  SlamDown [Header 1 (Text "foo")] -> trace "matched!"
  _                                -> trace "did not match!"

import Text.Markdown.SlamDown.Pretty 

-- rendering
(trace <<< prettyPrintMd <<< parseMd) "# foo"
```

### API

See the [module documentation](docs/).

### Tests

The tests use [purescript-strongcheck](http://github.com/purescript-contrib/purescript-strongcheck) to verify that an arbitrary `SlamDown` document can be rendered as a `String` and then parsed to a `SlamDown` equal to the original.

## Features

In general, SlamDown is a subset of [CommonMark](http://spec.commonmark.org/), supporting the following features:

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

In addition, a few simplifications have been made to exclude some more obscure and redundant cases. This makes the parser much simpler and cleans up the code considerably (if you really need full compatibility, PRs are welcome!).

The parser produces an ADT which describes the structure of the document.

## Extensions to CommonMark

SlamDown extends CommonMark in several ways:

 * **Evaluated code blocks** &mdash; These code blocks are evaluated by the Markdown application and results of the evaluation are inserted into the document. This is similar to an image which is essentially an evaluated link (restricted to image links).
 * **Form Elements** &mdash; Form elements may be named, given default values, and embedded into a document. The host application decides what to do with them, if anything &mdash; beyond rendering them as forms.

### Code Evaluation

Fenced code blocks may be evaluated by prefixing them with an exclamation point character (`!`). The result of evaluating the code is then inserted into the document at that location.

For example, in a document supporting evaluation of Javascript, the inline code block !`1 + 2` would be evaluated and the resulting number (`3`) would be inserted into the document at that location.

Code evaluation may be used for inline or block-level fenced code. 

If an info-string is specified, the evaluation must use the specified language or error. If no info-string is specified, the default language understood by the Markdown application is used.

**Note**: This library does not provide any support for evaluation of code, and the code snippets are treated as completely opaque, but the documentation does define *semantics* for how these blocks interact with other elements and with the rendering of the document.

Code evaluation is provided by the `eval` function, which takes an evaluation function, and replaces code blocks and inline code with the evaluated content. More general evaluation functions (for example, evaluating _form elements_ can be constructed using the `everywhere` function to traverse the `SlamDown` ADT).

### Form Elements

Form fields allow the collection of named, weakly-typed user-input. All fields may have default values, and while it's possible to hard-code all static data and default values for all fields, it is also possible to use this feature in conjunction with code evaluation, so that data and default values are generated dynamically by evaluating code.

Although the suggested syntax has been modified to be more consistent (with respect to default values) and extended to include other types (e.g. dates and times), original credit to [Yevgeniy Brikman](http://brikis98.blogspot.com/2011/07/proposal-extend-markdown-syntax-to.html) for the idea of allowing forms in Markdown.

#### Text Input

```
name = ________

name = ________ (default)

name = ________ (!`...`)
```

If code evaluation is used to produce the default, then the snippet must evaluate to textual content.

#### Numeric Input

```
age = #________

age = #________ (29)

age = #________ (!`...`)
```

If code evaluation is used to produce the default, then the snippet must evaluate to numeric content.

#### Radio Buttons

```
sex = (x) male () female

sex = (!`...`) !`...`
```

If code evaluation is used to produce the values, then the first snippet must evaluate to a label, and the second snippet must evaluate to a list of labels.

#### Checkboxes

```
phones = [] Android [x] iPhone [x] Blackberry

phones = [!`..`] !`...`
```

If code evaluation is used to produce the values, then both snippets must evaluate to a list of strings. The second list defines the checkboxes' labels and the first defines which checkboxes are to be checked. Checkboxes whose labels are included in the first list will be checked.

#### Dropdowns

```
city = {BOS, SFO, NYC}

city = {BOS, SFO, NYC} (NYC)

city = {!`...`} (!`...`)
```

If code evaluation is used to produce the set of choices, the snippet must evaluate to a list of labels. If code evaluation is used to produce the default choice, the snippet must evaluate to a label.

#### Date

```
start = __ - __ - ____

start = __ - __ - ____ (2015-06-06)

start = __ - __ - ____ (!`...`)
```

If code evaluation is used to produce the default, the snippet must evaluate to a date.

#### Time

```
start = __ : __

start = __ : __ (22:32)

start = __ : __ (!`...`)
```

If code evaluation is used to produce the default, the snippet must evaluate to a time.

#### DateTime

```
start = __ - __ - ____ __ : __

start = __ - __ - ____ __ : __ (2015-06-06T12:00)

start = __ - __ - ____ __ : __ (!`...`)
```

If code evaluation is used to produce the default, the snippet must evaluate to a date / time.

#### Required Fields

```
zip* = ________ (12345)
```

#### Fields with spaces in the label

```
[first name] = ________ (default)

[zip code]* = ________ (12345)
```
