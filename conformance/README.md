# Parser conformance: htmldom.core vs a real Blink browser

Differential testing against a real browser, because unit tests can only
check what someone thought to assert. This parses the same markup through
`htmldom.core/parse-into-document` → `kotoba.wasm.dom/tree` and through a
real headless Chrome, and compares the **tree shape** the two built: element
tag names and nesting depth in document order, plus text nodes normalised
for whitespace, plus comments.

```bash
nbb --classpath "src:../dom-gpu/src:../cssom/src" conformance/run.cljs \
  [--browser "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"] \
  [--only table/] [--verbose] [--ledger path/to/ledger.edn]
```

`--verbose` prints the tree fragment either side of each first divergence,
which is usually enough to attribute a failure without opening a REPL.

This exists because the repo's README says *"WHATWG HTML compatibility: not
a goal (trusted subset only)"* and nothing ever measured how far from a
browser that actually is. The sibling `kotoba-lang/cssom` repo measures its
**layout** the same way and that harness has repeatedly found real bugs;
this is the parser's own equivalent, and the first run found two.

## Result — 2026-08-04, first run

**Tree shape: 82/100 = 82%** on the first run against the unmodified parser.
After two fixes the harness justified (below): **88/100 = 88%**.

The number that matters is not 88 but the map underneath it. Per group:

| group | | | |
|---|---|---|---|
| attribute | 11/11 | 100% | |
| eof | 8/8 | 100% | after the tag-open fix; was 3/8 |
| form | 4/4 | 100% | |
| list | 6/6 | 100% | |
| rawtext | 9/9 | 100% | |
| whitespace | 5/5 | 100% | |
| formatting | 10/11 | 91% | |
| entity | 7/8 | 88% | after the nbsp fix; was 6/8 |
| paragraph | 6/7 | 86% | |
| misc | 4/5 | 80% | |
| selfclose | 4/5 | 80% | |
| **table** | **12/16** | **75%** | the largest single gap |
| **comment** | **1/4** | **25%** | structurally impossible today |

**Attributes** (a secondary axis, see below): 236/237 comparable elements
carry exactly the browser's attribute map.

82% is higher than the repo's own framing would predict, and that is a real
finding: optional end tags, implied `<tbody>`, misnested formatting, raw
text, entity decoding and attribute tokenization are in far better shape
than "trusted subset only" suggests — the parser has quietly grown most of
an in-body insertion mode. The remaining 12 failures are not scattered; they
are four causes.

## The four causes, in order of size

**1. Table foster parenting (3 cases) — not implemented.** Non-table content
between `<table>` and its first row must be moved *before* the table.
`<table>stray<tr><td>x</table>` gives a browser `#text "stray"` followed by
`table`; here it stays nested inside the table. Same for `<div>` and for
formatting elements. `htmldom.core`'s own namespace commentary already lists
"foster parenting (in-table)" as deferred to L3, so this is a measured
confirmation of a documented cut rather than news — but it is now sized.

**2. Comments have no node type (3 cases).** `kotoba.wasm.dom` knows only
`:element` and `:text`; `tokenize` therefore discards comments entirely.
Every case with a comment in it diverges, and the divergence cascades —
which is why the harness special-cases the category (see
`divergence-category`) instead of reporting the shifted text that follows.
Fixing this needs a change in `kotoba-lang/dom-gpu`, not here.

**3. `in table` insertion mode is incomplete beyond `<tbody>` (1 case).**
`<table><td>x</table>` should synthesise `tbody` *and* `tr`;
`maybe-insert-implicit-tbody` covers only a `<tr>` whose parent is the
table, and says so in its docstring.

**4. Individually rare spec behaviours (5 cases).** `</p>` with no open `<p>`
inserts an empty one; `</br>` is treated as `<br>`; `<a>` inside `<a>`
closes the outer one (the `a`/`nobr` special case, listed as deferred in the
namespace commentary); `<body>` in flow content is ignored (there is no
html/head/body model at all — an L3 architectural change); `&notit;` expands
to `¬it;` because a browser matches the longest named reference even without
a semicolon.

## What the two parser fixes were, and why they were fixes

Both were found by this harness, both have the evidence recorded in a code
comment at the change site, and both left the 143-test suite at 0 failures.

**Bare `<` in prose was fabricating elements and deleting text.**
`<p>1 < 2 and 3 > 4</p>` tokenized to a start tag named `2` with an
attribute `and`, and `2 and 3 >` disappeared from the document: the scan
took the `<` before ` 2` as a tag open and found the `>` after `3` as its
terminator. HTML5's tag-open state only enters markup for ASCII alpha, `/` +
alpha, `!` or `?`; anything else emits the `<` as an ordinary character.
`markup-start?` now implements exactly that, and `tokenize` keeps a literal
`<` inside the surrounding text run so it produces one text node rather than
splitting it. The same change made an unterminated tag at EOF drop the
partial tag (HTML5's `eof-in-tag`) instead of leaking `div class="x` into
the document as visible text. Four cases, `eof` 3/8 → 8/8.

**`&nbsp;` was being decoded and then destroyed.** Text collapsing used
`[^\S\n]` — "everything `\s` matches except newline" — and `\s` also matches
U+00A0. So `&nbsp;`, correctly decoded to U+00A0 a moment earlier, was
rewritten straight back to a collapsible U+0020: the browser reported
codepoints 97/160/98 for `<p>a&nbsp;b</p>` and this parser 97/32/98. That
destroys the only thing `&nbsp;` exists to do, and CSS is explicit that
U+00A0 is not collapsible white space. The class is now spelled out as
HTML's own ASCII whitespace set minus the newline this parser deliberately
keeps.

Deliberately **not** fixed: everything in the four causes above, plus the
one attribute divergence — `@click="f"` becomes `click="f"`, because
`attrs`' name pattern requires an ASCII-alpha/`_`/`:` first character while
the spec forbids only whitespace, `/`, `>` and `=` in an attribute name.
Real in the wild (Vue, Alpine, htmx) but the fix means widening the single
most load-bearing regex in the file, and this axis is secondary; it is
recorded here rather than changed on the way past.

## How the browser is read

The same way the cssom harness reads layout, for the same reason: no CDP
client, no Playwright, no driver.

One page, one browser launch, `--headless=old --dump-dom` written to a
**file**. Both of those are measured constraints, not preferences.
`--headless=new --dump-dom` prints nothing at all and bare `--headless`
never returns; and Chromium's child processes inherit stdout and hold it
open after the parent dies, so a **pipe never reaches EOF** and the reader
hangs forever — `spawnSync`'s own `:timeout` does not save you, because it
kills the parent and then still waits on the pipe. `timeout -s KILL` bounds
the run, because headless Chromium writes its dump and then ignores SIGTERM.
The page script base64s its result into a `<pre>` so no case's markup can
break the block being read back.

The one structural difference from the cssom harness is that each case is
injected with **`innerHTML` on a detached `<div>`** rather than written into
the page source. That is not cosmetic:

- `innerHTML` on a `<div>` *is* the HTML fragment parsing algorithm with a
  `<div>` context element — the same insertion modes, the same foster
  parenting, the same adoption agency — which is precisely what
  `parse-into-document` does (it parses into a `:document` root; there is no
  html/head/body synthesis on either side, so depth 0 means the same thing).
- It **isolates** the cases. Written into page source, an unterminated
  `<table>` keeps the "in table" insertion mode across everything after it
  and swallows the rest of the corpus, measurement script included. Half
  this corpus is deliberately malformed, so isolation is a correctness
  requirement.
- Scripts inserted via `innerHTML` never execute, so the raw-text cases
  cannot run anything.

The corpus goes into the page base64-encoded for the same reason the result
comes back that way: a case containing `</script>` would otherwise end the
element carrying it.

## Normalisation — and one harness bug it caught

Both sides are reduced by the *same* function. Two steps are worth naming
because getting them wrong scores the harness instead of the parser.

**Adjacent text nodes are merged first, on both sides.** A browser appends
characters to the text node already at the insertion point, so a token it
drops in between leaves one text node; htmldom emits one per `:text` token
and never merges. Before this step `<p>a</b>b</p>` was scored as a
divergence — `#text "a"`, `#text "b"` against `#text "ab"` — when the two
trees are `Node.normalize()`-identical and no consumer can tell them apart.
That was a harness bug, not a parser bug, and fixing it moved the first run
83% → 84% *before* any parser change. Merging happens on the raw text,
before whitespace normalisation, so the real content divergence it was
hiding stays visible: `<p>a < b</p>` still failed afterwards, correctly.

**Whitespace is collapsed but not trimmed.** Whitespace-only text nodes stay
in the comparison as their own `#ws` item, because where such a run lands is
genuine parser behaviour — this parser only started keeping them at all on
2026-08-03 (`fix(core): keep whitespace-only text runs`), and `<a>one</a>\n
<a>two</a>` renders as `one two` precisely because the run survives. The
collapsing class is ASCII whitespace only, deliberately **not** the regex
`\s` class, which would also match U+00A0 and erase the entity cases the
harness is there to measure. (It very nearly did: the parser bug above was
only visible because this class was written narrowly first.)

## What is excluded from comparison, and why

- **`style` attributes.** `apply-attrs` folds them into `:style-inline` /
  `:style-inline-important` plus a parsed map for the cascade — the reason
  this repo contains a `calc()` evaluator. Comparing a parsed map against a
  browser's raw string would score the cascade bridge, not the parser.
- **`default-value` / `default-checked` / `default-selected`, and `value` on
  `<select>`/`<textarea>`, `selected` on `<option>`.** Synthesised by
  `initialize-form-defaults`. In a browser these are DOM *properties*, never
  content attributes, so the oracle cannot have them and their absence is
  not a divergence.
- **Exact whitespace inside text.** `<pre>` and `<textarea>` preserve runs
  verbatim on both sides, but the comparison collapses them, so a
  divergence in how many spaces survive is invisible here. That difference
  is a *layout* question — `white-space` is a CSS property — and the cssom
  harness is where it becomes measurable. What this harness does check is
  that the whitespace-only *nodes* exist in the same places.
- **Element identity beyond tag name.** Namespaces (SVG/MathML) are not in
  the corpus at all; this parser has no namespace concept.

## Adding cases

Append to `cases.edn`. Keep the markup exact — incidental indentation inside
a case's `:html` becomes real whitespace text nodes on both sides, so
whitespace should only appear where a case is deliberately testing it. A
failing case is a **measurement, not a bug report**: several groups here are
known scope cuts and are in the corpus precisely so the number tells the
truth about them.
