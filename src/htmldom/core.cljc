(ns htmldom.core
  "Small trusted HTML subset parser for the kotoba-only browser R0.

   This intentionally does not claim WHATWG HTML compatibility. It only bridges
   simple HTML-like documents into kotoba.wasm.dom.

   Split out of kotoba-lang/browser (ADR-2607051140). Not to be confused with
   kotoba-lang/html, an unrelated Hiccup-compatible EDN HTML renderer (data ->
   HTML text); this namespace goes the other direction: parses HTML text into
   a kotoba.wasm.dom document."
  (:require [clojure.string :as str]
            [cssom.core :as css]
            [kotoba.wasm.dom :as dom]))

(def void-tags
  #{"area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta"
    "param" "source" "track" "wbr"})

;; ---- inline style="..." -- parsed by cssom, not here ----
;;
;; An inline `style="..."` attribute IS a CSS declaration block, so it is
;; parsed by the CSS parser: `cssom.core/parse-declarations-with-importance`,
;; the same entry point a `<style>` rule body goes through. This namespace
;; only decides WHERE a declaration block comes from (an attribute on an
;; element), never what one means.
;;
;; It used to mean both. This file carried its own ~330-line copy of that
;; parser -- the calc() pipeline, five shorthand expanders, the numeric/px
;; coercion, the `!important` regex -- justified (in the comments now
;; deleted) as "consistent with this codebase's existing convention of
;; independently re-implementing a handful of small, identically-scoped
;; helpers across repos rather than introducing a cross-repo dependency for
;; them, e.g. each repo's own `parse-number`/`truthy-attr?`". That
;; justification stopped being true as the copy grew, and the two copies
;; then drifted exactly the way two copies do: cssom taught its parser to
;; expand `margin`/`padding` into per-side longhands, this copy was never
;; told, and so a stylesheet's `padding: 12px` produced five keys while an
;; inline `style="padding: 12px"` produced one -- while `margin: 4px 8px`
;; inline produced the raw STRING "4px 8px", which the per-side box model
;; cannot read at all. Measured through the real pipeline by
;; kotoba-lang/cssom's conformance harness against a headless Blink: it was
;; the single largest cascade-attributed cause on the computed-style axis
;; (`:box/nested-padding`, `:box/border-box-sizing`,
;; `:box/padding-shifts-content`, `:box/border-and-padding-together`,
;; `:page/card`, `:inline/inline-block-with-padding`, ...).
;;
;; The drift was not limited to margin/padding either: cssom's parser gives
;; `content`/`counter-reset`/`counter-increment` their own parsing (and
;; drops an unparseable one rather than storing an unusable value), which
;; this copy never did, so those three properties also meant different
;; things inline than in a rule.
;;
;; The direction of the new dependency is the one a real browser has: the
;; HTML parser hands the style attribute's text to the CSS parser. cssom
;; does not depend on htmldom (only its conformance harness reads this
;; namespace, off the classpath), so there is no cycle -- and both repos'
;; CI already checked the other out as a sibling.

(defn parse-style
  "Parses a raw inline `style=\"...\"` attribute's text into a `{property
   value}` map, identically to a `<style>` rule body with the same text.

   Shorthands expand into their longhands (`padding: 12px` gives
   `:padding` plus the four `:padding-*` sides; likewise `border`,
   `text-shadow`, `box-shadow`, `outline`, `font`), values are coerced
   (`12px` -> 12, `calc(2 * 8px)` -> 16, a unitless `line-height` stays a
   ratio string), and a trailing `!important` is stripped from the value --
   all of it by `cssom.core`, see this section's own header comment for why
   none of it lives here any more. `style-importance` is the companion that
   reports WHICH properties were marked `!important` (needed so
   `cssom.core` can rank them against `!important` rule-based declarations
   -- real CSS's importance/cascade-origin step)."
  [style-text]
  (css/parse-declarations style-text))

(defn style-importance
  "The set of property keywords whose declaration in `style-text` (a raw
   inline `style=\"...\"` attribute's text) ended in `!important`.

   A shorthand marked `!important` marks every longhand it expands into --
   `style=\"padding: 4px !important\"` reports all five padding keys, not
   just `:padding` -- because those longhands are what the cascade actually
   ranks, and real CSS applies the shorthand's importance to each of them.

   Deliberately a SEPARATE function/attr (`:style-inline-important`, see
   `apply-attrs`) rather than changing `parse-style`'s own `{property
   value}` return shape to also carry importance inline -- `:style-inline`
   has real consumers elsewhere (kotoba-lang/browser's `dom_bridge.cljc`,
   and several of that repo's own tests) that expect a plain value map."
  [style-text]
  (into #{}
        (keep (fn [[k {:keys [important?]}]] (when important? k)))
        (css/parse-declarations-with-importance style-text)))

(def ^:private named-char-refs
  "A small, pragmatic subset of HTML5 named character references: the five
   XML-predefined entities (universally supported, unambiguous, no lookup
   table strictly needed) plus a handful of the other named entities most
   likely to show up in real-world text content. This is deliberately NOT the
   full ~2000-entry HTML5 named character reference table -- implementing
   that would be wildly out of scope for a trusted-HTML-subset parser. Any
   named entity not in this table is left as literal text (`&foo;` stays
   `&foo;`) rather than guessed at, matching this project's existing
   degrade-don't-guess convention (the same one `cssom.core`'s own value
   parsing applies to unsupported CSS values)."
  {"amp" "&" "lt" "<" "gt" ">" "quot" "\"" "apos" "'"
   "nbsp" "\u00A0" "copy" "\u00A9" "reg" "\u00AE" "trade" "\u2122"
   "mdash" "\u2014" "ndash" "\u2013" "hellip" "\u2026"
   ;; Added after a real page shape in kotoba-lang/cssom's conformance
   ;; corpus (a footer reading `&copy; 2026 <a>Privacy</a> &middot;
   ;; <a>Terms</a>`) rendered the literal text "&middot;" on screen: the
   ;; separator dot, the typographic quotes, the arrows and the primes are
   ;; the entities that actually appear in ordinary page furniture, and a
   ;; missing one is not a subtle degradation -- it paints the source text.
   "middot" "\u00B7" "bull" "\u2022" "dagger" "\u2020" "sect" "\u00A7"
   "para" "\u00B6" "deg" "\u00B0" "plusmn" "\u00B1" "times" "\u00D7"
   "divide" "\u00F7" "frac12" "\u00BD" "sup2" "\u00B2" "sup3" "\u00B3"
   "laquo" "\u00AB" "raquo" "\u00BB" "lsquo" "\u2018" "rsquo" "\u2019"
   "ldquo" "\u201C" "rdquo" "\u201D" "prime" "\u2032" "Prime" "\u2033"
   "larr" "\u2190" "uarr" "\u2191" "rarr" "\u2192" "darr" "\u2193"
   "harr" "\u2194" "check" "\u2713" "cross" "\u2717" "star" "\u2606"
   "euro" "\u20AC" "pound" "\u00A3" "yen" "\u00A5" "cent" "\u00A2"
   "shy" "\u00AD" "ensp" "\u2002" "emsp" "\u2003" "thinsp" "\u2009"
   "minus" "\u2212" "ne" "\u2260" "le" "\u2264" "ge" "\u2265"
   "infin" "\u221E" "asymp" "\u2248" "micro" "\u00B5" "ordm" "\u00BA"
   ;; A breadcrumb (`Home &rsaquo; Docs &rsaquo; Page`) painted the literal
   ;; text "&rsaquo;" -- found by kotoba-lang/cssom's conformance corpus the
   ;; first time it contained one. The angle quotes and the remaining
   ;; typographic separators complete the page-furniture set.
   "rsaquo" "\u203A" "lsaquo" "\u2039" "sbquo" "\u201A" "bdquo" "\u201E"
   "Dagger" "\u2021" "permil" "\u2030" "oline" "\u203E" "frasl" "\u2044"
   "lowast" "\u2217" "loz" "\u25CA" "spades" "\u2660" "clubs" "\u2663"
   "hearts" "\u2665" "diams" "\u2666" "not" "\u00AC" "sup1" "\u00B9"
   "frac14" "\u00BC" "frac34" "\u00BE" "iquest" "\u00BF" "iexcl" "\u00A1"
   "brvbar" "\u00A6" "uml" "\u00A8" "macr" "\u00AF" "acute" "\u00B4"
   "cedil" "\u00B8" "curren" "\u00A4" "szlig" "\u00DF"})

(defn- codepoint->str
  "A Unicode codepoint (possibly outside the BMP, e.g. an emoji) as a string,
   correctly emitted as a surrogate pair where needed."
  [cp]
  #?(:clj (String. (Character/toChars (int cp)))
     :cljs (js/String.fromCodePoint cp)))

(def ^:private windows-1252-c1-remap
  "WHATWG spec's 'numeric character reference end state' table: a numeric
   character reference naming a codepoint in the C1 control range
   (0x80-0x9F) is NOT emitted as that raw control character -- it is
   reinterpreted through this fixed Windows-1252 remap instead. This is
   the textbook 'smart quotes turn into garbage' bug: real-world HTML
   authored against (or exported from apps that assume) Windows-1252 --
   Word documents, legacy CMSes -- commonly emits e.g. `&#146;` meaning a
   right single quote, never the invisible C1 control character 0x92 it
   names literally. Five codepoints in this range (0x81/0x8D/0x8F/0x90/
   0x9D) have no Windows-1252 mapping and are deliberately absent here,
   matching the spec exactly -- those five fall through to this fn's
   caller's default (raw codepoint) unchanged."
  {0x80 0x20AC, 0x82 0x201A, 0x83 0x0192, 0x84 0x201E, 0x85 0x2026,
   0x86 0x2020, 0x87 0x2021, 0x88 0x02C6, 0x89 0x2030, 0x8A 0x0160,
   0x8B 0x2039, 0x8C 0x0152, 0x8E 0x017D,
   0x91 0x2018, 0x92 0x2019, 0x93 0x201C, 0x94 0x201D, 0x95 0x2022,
   0x96 0x2013, 0x97 0x2014, 0x98 0x02DC, 0x99 0x2122, 0x9A 0x0161,
   0x9B 0x203A, 0x9C 0x0153, 0x9E 0x017E, 0x9F 0x0178})

(defn- numeric-char-ref->str
  "Decode the digits of a numeric character reference (`radix` 10 or 16) to
   the corresponding Unicode codepoint's string, or nil if the digits don't
   parse or name a codepoint outside the valid Unicode range (including the
   surrogate-only range, which is never a valid scalar value) -- callers fall
   back to leaving the reference as literal text, same as an unrecognized
   named entity. A codepoint in the C1 control range (0x80-0x9F) is
   reinterpreted through `windows-1252-c1-remap` first (see its own
   docstring); codepoint 0 becomes U+FFFD REPLACEMENT CHARACTER, matching
   the same spec algorithm's explicit null-character rule."
  [digits radix]
  (try
    (let [cp #?(:clj (Long/parseLong digits radix) :cljs (js/parseInt digits radix))]
      (when (and (<= 0 cp 0x10FFFF) (not (<= 0xD800 cp 0xDFFF)))
        (codepoint->str (cond
                          (zero? cp) 0xFFFD
                          (contains? windows-1252-c1-remap cp) (get windows-1252-c1-remap cp)
                          :else cp))))
    (catch #?(:clj Exception :cljs :default) _ nil)))

(def ^:private char-ref-pattern
  ;; Named references require the trailing `;` (see decode-entities'
  ;; docstring for why). Numeric references' trailing `;` is optional: unlike
  ;; named entities, a numeric reference is self-delimiting by digit class,
  ;; so no lookup table is needed to know where it ends, and real browsers
  ;; accept it either way.
  #"&(?:([a-zA-Z][a-zA-Z0-9]*);|#(\d+);?|#[xX]([0-9a-fA-F]+);?)")

(defn decode-entities
  "Decode HTML character references in `s`: named entities (a pragmatic
   subset, see `named-char-refs`) and numeric character references
   (`&#65;` decimal, `&#x41;`/`&#X41;` hex) to their Unicode codepoint.

   Simplification (documented, deliberate): a named reference's trailing `;`
   is required -- omitting it is genuinely ambiguous without the full
   ~2000-entry HTML5 named character reference table this project doesn't
   implement (e.g. is `&notit` a truncated `&not;it` or literally `&notit`?).
   A numeric reference's trailing `;` is optional, since digits are
   self-delimiting (no ambiguity, so no reason to be stricter than browsers
   are). Anything not recognized -- an unknown named entity, or numeric
   digits naming an invalid codepoint -- is left as literal, unmodified text
   rather than guessed at, matching this project's degrade-don't-guess
   convention."
  [s]
  (if (or (nil? s) (not (str/index-of s "&")))
    s
    (str/replace
     s
     char-ref-pattern
     (fn [[whole named decimal hex]]
       (or (cond
             named (get named-char-refs named)
             decimal (numeric-char-ref->str decimal 10)
             hex (numeric-char-ref->str hex 16))
           whole)))))

(defn- attrs
  [s]
  (->> (re-seq #"([A-Za-z_:][-A-Za-z0-9_:.]*)(?:\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s\"'>]+)))?" s)
       (map (fn [[_ k dq sq bare]]
              [(keyword (str/lower-case k))
               (if-let [v (or dq sq bare)]
                 (decode-entities v)
                 ;; Real WHATWG HTML tokenization: a valueless/bare
                 ;; attribute (`<input checked>`, no `=value` at all) has
                 ;; its value set to the empty string "" -- that's what
                 ;; Element.getAttribute() is spec-required to return.
                 ;; Previously this stored the Clojure boolean `true`
                 ;; instead, which survives UNMODIFIED all the way out to
                 ;; real page JS through dom-gpu's set-attribute (verbatim
                 ;; assoc) and browser's quickjs_wasm.cljc snapshot/
                 ;; JSON.stringify bridge, so
                 ;; `getAttribute('checked')` returned the literal string
                 ;; "true" instead of "" -- confirmed via direct REPL
                 ;; reproduction before touching source. Every
                 ;; `truthy-attr?` copy in this codebase (htmldom.core,
                 ;; browser/document_input.cljc, cssom/core.cljc) already
                 ;; has an explicit `(= "" v)` case alongside `(= true
                 ;; v)`, confirming "" was already the intended/expected
                 ;; internal representation everywhere this value is
                 ;; actually consumed -- `true` was simply never the
                 ;; correct value to produce here in the first place.
                 "")]))
       ;; Real HTML5 tokenization: the FIRST occurrence of a duplicate
       ;; attribute wins and every later one is a parse error, dropped --
       ;; `into {}` would instead let the LAST occurrence silently
       ;; overwrite it, since `assoc` on a repeated key always keeps the
       ;; newest value.
       (reduce (fn [acc [k v]] (if (contains? acc k) acc (assoc acc k v))) {})))

(defn- unquoted-value-tail?
  "True if the end of `s` (tag name + attribute text, no wrapping <, >, or
   leading '/') is inside an in-progress UNQUOTED attribute value -- i.e.
   per real HTML5 tokenization, a trailing '/' at this position is
   literal value content, not a self-closing marker. `/` is ONLY a
   self-closing marker while in the tag-name, before-attribute-name,
   attribute-name, or after-attribute-value-quoted states; inside an
   unquoted value it has no special meaning at all and is simply
   appended, exactly like any other character, until whitespace or the
   tag's own '>' ends the value. Scans left-to-right with the same
   quote-tracking shape as tag-close-index above: whitespace always
   exits value/after-= position (back to \"before attribute name\"); a
   quote right after '=' opens a quoted value (its own contents are
   never \"unquoted\"); any other character right after '=' begins an
   unquoted value that continues until the next whitespace/quote/=."
  [s]
  (let [len (count s)]
    (loop [pos 0 quote nil after-eq? false unquoted? false]
      (if (>= pos len)
        unquoted?
        (let [c (subs s pos (inc pos))]
          (cond
            quote (recur (inc pos) (when-not (= c quote) quote) false false)
            (re-matches #"[ \t\n\r]" c) (recur (inc pos) nil false false)
            (= c "=") (recur (inc pos) nil true false)
            (and after-eq? (or (= c "\"") (= c "'"))) (recur (inc pos) c false false)
            after-eq? (recur (inc pos) nil false true)
            unquoted? (recur (inc pos) nil false true)
            :else (recur (inc pos) nil false false)))))))

(defn- token
  [raw]
  (if (str/starts-with? raw "<")
    (let [body (subs raw 1 (dec (count raw)))
          closing? (str/starts-with? body "/")
          body (str/trim (if closing? (subs body 1) body))
          ;; A trailing `/` right before the tag's `>` is stripped from the
          ;; tag/attribute text (matching real HTML5, which ignores a stray
          ;; trailing slash on any start tag) -- but ONLY when it is genuinely
          ;; a self-closing marker, not when it directly continues an
          ;; UNQUOTED attribute value that happens to end right at the `>`
          ;; (a real, common pattern for URLs, e.g. `href=http://example.com/`
          ;; or `src=http://example.com/img/`). Per real HTML5 tokenization,
          ;; `/` has no special meaning inside an unquoted value at all --
          ;; unconditionally stripping it here silently truncated the value
          ;; by one character, confirmed via direct REPL reproduction before
          ;; this fix. This must happen BEFORE the tag name is extracted, and
          ;; self-closing status is determined SOLELY by void-tag membership,
          ;; not by the presence of this slash: real HTML5 only treats actual
          ;; void elements (`<br/>`, `<img/>`, ...) as self-closing. A
          ;; trailing `/` on an ordinary element like `<div/>` is ignored --
          ;; the element still needs, and will be matched against, a real
          ;; closing tag later.
          body (str/trim (if (and (str/ends-with? body "/")
                                   (not (unquoted-value-tail? (subs body 0 (dec (count body))))))
                            (subs body 0 (dec (count body)))
                            body))
          [tag attr-text] (str/split body #"\s+" 2)
          tag (str/lower-case (or tag ""))
          self? (contains? void-tags tag)]
      (if closing?
        {:type :end :tag tag}
        {:type :start :tag tag :attrs (attrs (or attr-text "")) :self? self?}))
    ;; Whitespace COLLAPSING is deliberately NOT done here anymore -- it
    ;; depends on whether a <pre> ancestor is currently open, which this
    ;; stateless, stack-free tokenizing pass has no way to know (unlike
    ;; parse-into-document, which already tracks the open-element stack for
    ;; auto-closing). This fn keeps the ORIGINAL, uncollapsed text in the
    ;; token; parse-into-document's own `:text` case now does the
    ;; collapsing, conditionally, once it can actually check the stack. The
    ;; blank-check stays here unconditionally, though: a run of ONLY
    ;; whitespace between tags is dropped the same way regardless of <pre>
    ;; context either way (collapsing an all-whitespace string never changes
    ;; whether it's blank), so this filtering doesn't need to move.
    ;; Whitespace-only runs are KEPT. They are the space between inline
    ;; elements -- `<a>one</a>\n  <a>two</a>` renders as `one two` in every
    ;; browser, and dropping the run here made the words touch, with no way
    ;; for layout to recover the gap. Layout is where a whitespace-only text
    ;; child gets discarded when it would form a stray row of its own
    ;; between blocks (cssom.layout's inline-runs), because that decision
    ;; needs the box tree this tokenizer cannot see.
    (when-not (empty? raw)
      {:type :text :text (decode-entities raw)})))

(def ^:private raw-text-tags
  "Elements whose content real HTML parsers scan as literal raw text rather
   than markup: `<`/`>`/quotes inside them are not interpreted, and only the
   first matching end tag terminates them.

   `script`/`style` are true \"raw text\" elements per the WHATWG spec.
   `title`/`textarea` are technically \"RCDATA\" -- the one difference being
   that character references (e.g. `&amp;`) are still decoded inside RCDATA
   even though tags aren't parsed there either. See `rcdata-tags` for where
   that distinction is now applied (tokenize decodes entities in the raw text
   it captures only for the tags in that subset)."
  #{"script" "style" "title" "textarea"})

(def ^:private rcdata-tags
  "The subset of `raw-text-tags` that are \"RCDATA\" rather than true \"raw
   text\" per the WHATWG spec: `title`/`textarea`. Tags are not parsed inside
   either kind of element (both are scanned literally up to their real
   closing tag), but character references ARE decoded inside RCDATA and are
   NOT decoded inside true raw text. `script`/`style` are true raw text: a JS
   string literal or CSS content containing `&amp;` is genuinely raw and must
   not become `&`."
  #{"title" "textarea"})

(defn- tag-close-index
  "Index of the real closing '>' for a start/end tag beginning at `lt` (the
   index of the tag's own '<'), scanned from `lt`. A naive index-of the next
   '>' breaks on a real, common, valid pattern: a literal, UNESCAPED '>'
   inside a single- or double-quoted attribute value (e.g. `title=\"a > b\"`,
   or the extremely common inline `onclick=\"if (a>b) foo()\"`) is real
   HTML5 -- quoted attribute values are scanned verbatim up to their own
   matching quote, `>` has no special meaning inside them at all. A naive
   scan truncates the tag at that inner '>', corrupting `:attrs` (losing
   every attribute after it) and leaking the quoted value's own remainder as
   a stray text node, confirmed via direct REPL reproduction before this
   fix. This tracks whether the scan is currently inside an open quote span
   and only treats a '>' outside of one as the tag's real terminator.
   Returns nil if no real closing '>' exists in the rest of the input
   (matches the old naive index-of's own nil-for-unmatched contract,
   including an unterminated quote -- the loop then never exits the quote
   state before reaching the end of input)."
  [html lt]
  (let [len (count html)]
    (loop [pos (inc lt) quote nil]
      (if (>= pos len)
        nil
        (let [c (subs html pos (inc pos))]
          (cond
            quote (recur (inc pos) (when-not (= c quote) quote))
            (or (= c "\"") (= c "'")) (recur (inc pos) c)
            (= c ">") pos
            :else (recur (inc pos) quote)))))))

(defn- raw-text-tag-name
  "If html[lt..gt] (inclusive angle brackets, gt = index of the tag's '>')
   opens a raw-text element, return its lower-cased tag name. Returns nil for
   closing tags, comments/doctypes/PIs, and any ordinary element."
  [html lt gt]
  (let [body (str/trim (subs html (inc lt) gt))]
    (when-not (or (str/starts-with? body "/")
                  (str/starts-with? body "!")
                  (str/starts-with? body "?"))
      (raw-text-tags (str/lower-case (first (str/split body #"[\s/>]" 2)))))))

(defn- raw-text-close-index
  "Index of the '<' beginning the literal, case-insensitive closing tag
   `</tag` for `tag`, scanned from `from` in `html`. Matches real raw-text
   parsing: any '<'/'>' or nested same-name start tag in between is ignored,
   only the first end-tag boundary terminates the element. Returns
   (count html) if unterminated (rest of input becomes the element's text,
   as browsers do for an unterminated <script>/<style> at EOF)."
  [html tag from]
  (let [lower (str/lower-case html)
        len (count html)
        needle (str "</" (str/lower-case tag))
        needle-len (count needle)
        boundary-chars #{" " "\t" "\n" "\r" ">" "/"}]
    (loop [pos from]
      (if-let [idx (str/index-of lower needle pos)]
        (let [after (+ idx needle-len)]
          (if (or (>= after len)
                  (contains? boundary-chars (subs lower after (inc after))))
            idx
            (recur (inc idx))))
        len))))

(defn- char-at
  "The single character at `i` as a one-character string, or nil past the
   end. `subs`-based rather than `nth`/`get` because those return a
   java.lang.Character on the JVM and a one-character string in
   ClojureScript, which cannot be compared the same way in `.cljc`."
  [s i]
  (when (< i (count s)) (subs s i (inc i))))

(defn- markup-start?
  "Whether the `<` at `lt` actually OPENS markup, per real HTML5's tag-open
   state (WHATWG 13.2.5.6): only ASCII alpha (start tag), `/` followed by
   ASCII alpha (end tag), `!` (markup declaration / comment / doctype) and
   `?` (bogus comment) do. For ANY other next character the spec emits the
   `<` as an ordinary character and reconsumes in the data state -- a `<`
   in running prose is literal text, not a broken tag.

   Measured before this existed, via the conformance harness (see
   conformance/README.md, `eof/unmatched-lt`): `<p>1 < 2 and 3 > 4</p>`
   tokenized to a start tag named `2` with an attribute `and`, and the text
   `2 and 3 >` vanished from the document entirely -- because the scan took
   the `<` before ` 2` as a tag open and then found the `>` after `3` as its
   terminator. A real browser produces one text node, `1 < 2 and 3 > 4`.
   Bare `<` in prose (code snippets, arithmetic, `a < b`) is common enough
   that this was silent content loss, not an exotic edge case."
  [html lt]
  (let [c (char-at html (inc lt))]
    (boolean
     (and c (or (some? (re-matches #"[A-Za-z!?]" c))
                (and (= "/" c)
                     (when-let [d (char-at html (+ lt 2))]
                       (some? (re-matches #"[A-Za-z]" d)))))))))

(defn tokenize
  [html]
  (let [html (or html "")
        len (count html)
        ;; Text accumulates from `pos` and is only flushed when real markup
        ;; is reached, so a literal `<` (see `markup-start?`) stays INSIDE
        ;; the surrounding run and produces one text node rather than
        ;; splitting it -- which is also what a browser builds, since it
        ;; appends characters to the text node already at the insertion
        ;; point.
        flush (fn [acc end pos]
                (if (> end pos)
                  (if-let [t (token (subs html pos end))] (conj acc t) acc)
                  acc))]
    ;; `scan` is where the search for the next `<` resumes; `pos` is where
    ;; the pending text run began. They differ exactly when a `<` turned out
    ;; to be literal text.
    (loop [pos 0
           scan 0
           acc []]
      (let [lt (or (str/index-of html "<" scan) len)]
        (cond
          (>= lt len)
          (flush acc len pos)

          ;; Not a tag open at all (see `markup-start?`): the `<` is an
          ;; ordinary character. Keep it in the pending text run and carry
          ;; on looking for the next `<`.
          (not (markup-start? html lt))
          (recur pos (inc lt) acc)

          ;; HTML comment: scanned literally for its real `-->` terminator so
          ;; a `>` inside the comment body can't truncate it and corrupt the
          ;; rest of the token stream. Comments produce no token (discarded),
          ;; matching prior behavior.
          ;;
          ;; Real HTML5's two "abrupt-closing-of-empty-comment" forms --
          ;; `<!-->` and `<!--->` -- are complete, empty, immediately-
          ;; terminated comments (their own named parse error in the spec),
          ;; reusing the marker's OWN trailing `--` as the closing marker's
          ;; leading `--`. Searching from `lt + 4` (strictly after those two
          ;; dashes) can never see that overlap, so it always missed both
          ;; forms and searched for the NEXT literal `-->` anywhere later in
          ;; the document instead -- silently swallowing everything up to
          ;; (or, if none exists, all the way to end-of-input) as comment
          ;; content, discarding real markup wholesale. Confirmed via direct
          ;; REPL reproduction: `<main><p>before</p><!--><p>after</p></main>`
          ;; parsed to a `<main>` with only ONE child -- `<p>after</p>` and
          ;; its text node never existed in the tree at all. Searching from
          ;; `lt + 2` instead (right after the bare `<!`) lets the marker's
          ;; own `--` double as the closer for the two abrupt-closing forms,
          ;; while an ordinary `<!--content-->` is unaffected: the search
          ;; only matches early if the very next character really is `>`,
          ;; otherwise it keeps scanning forward to the real terminator
          ;; exactly as before.
          (str/starts-with? (subs html lt) "<!--")
          (let [acc (flush acc lt pos)
                end (str/index-of html "-->" (+ lt 2))
                next-pos (if end (+ end 3) len)]
            (recur next-pos next-pos acc))

          :else
          (let [acc (flush acc lt pos)
                gt (tag-close-index html lt)]
            (if (nil? gt)
              ;; A real tag open with no terminating '>' anywhere in the rest
              ;; of the input (including an attribute quote that never
              ;; closes). WHATWG's eof-in-tag parse error DROPS the partial
              ;; tag and everything the tokenizer had consumed into it, so
              ;; the input simply ends here. Measured (conformance case
              ;; `eof/unterminated-tag`): `<p>a</p><div class="x` produced a
              ;; visible text node reading `div class="x` -- raw markup
              ;; leaking into the rendered document -- where a browser shows
              ;; nothing at all.
              acc
              (let [raw (subs html lt (inc gt))]
                (if (or (str/starts-with? raw "<!") (str/starts-with? raw "<?"))
                  ;; doctype / processing instruction: discarded, as before.
                  (recur (inc gt) (inc gt) acc)
                  (let [raw-tag (raw-text-tag-name html lt gt)
                        open-tok (token raw)]
                    (if (and raw-tag (not (:self? open-tok)))
                      ;; Raw-text element: emit its real opening tag, then the
                      ;; verbatim (whitespace-preserving) text up to the real
                      ;; closing tag, then the real closing tag itself.
                      (let [close-idx (raw-text-close-index html raw-tag (inc gt))
                            text (subs html (inc gt) close-idx)
                            ;; WHATWG spec: if a <textarea>'s content begins
                            ;; with a single U+000A LINE FEED, that one
                            ;; character is silently dropped -- authoring
                            ;; convenience for the extremely common
                            ;; "<textarea>\nvalue" source-formatting habit
                            ;; (immediately newlining after the open tag).
                            ;; Scoped to `textarea` only, matching the spec --
                            ;; `title`/`script`/`style` get no such treatment.
                            text (if (and (= raw-tag "textarea") (str/starts-with? text "\n"))
                                   (subs text 1)
                                   text)
                            ;; RCDATA (title/textarea): entities decode even
                            ;; though tags don't parse. True raw text
                            ;; (script/style): verbatim, no decoding -- a JS
                            ;; string literal containing `&amp;` must stay
                            ;; `&amp;`, not become `&`.
                            text (if (contains? rcdata-tags raw-tag)
                                   (decode-entities text)
                                   text)
                            acc (cond-> acc open-tok (conj open-tok))
                            acc (if (seq text) (conj acc {:type :text :text text}) acc)]
                        (if (< close-idx len)
                          (let [close-gt (or (str/index-of html ">" close-idx) len)
                                next-pos (if (< close-gt len) (inc close-gt) len)]
                            (recur next-pos next-pos
                                   (conj acc {:type :end :tag raw-tag})))
                          (recur len len acc)))
                      (recur (inc gt) (inc gt)
                             (if open-tok (conj acc open-tok) acc)))))))))))))

(defn- apply-attrs
  [document id attrs]
  (reduce-kv
   (fn [document k v]
     (if (= k :style)
       (let [style (parse-style v)]
         (-> document
             (dom/set-attribute id :style-inline style)
             (dom/set-attribute id :style-inline-important (style-importance v))
             (dom/set-style id style)))
       (dom/set-attribute document id k v)))
   document
   attrs))

(defn- truthy-attr? [v]
  (or (= true v)
      (= "true" v)
      (= "" v)
      (and (string? v)
           (not (str/blank? v))
           (not= "false" (str/lower-case v)))))

(defn- set-attribute-if-missing
  [document id k v]
  (if (contains? (get-in document [:nodes id :attrs]) k)
    document
    (dom/set-attribute document id k v)))

(defn- text-content
  [document id]
  (let [node (get-in document [:nodes id])]
    (case (:node/type node)
      :text (:text node)
      :element (str/join "" (map #(text-content document %) (:children node)))
      "")))

(defn- descendant-node-ids
  ([document id]
   (descendant-node-ids document id #{}))
  ([document id visited]
   (when-not (contains? visited id)
     (let [visited (conj visited id)]
       (mapcat (fn [child-id]
                 (cons child-id (descendant-node-ids document child-id visited)))
               (get-in document [:nodes id :children]))))))

(defn- parent-node-id
  [document child-id]
  (some (fn [[node-id node]]
          (when (some #{child-id} (:children node))
            node-id))
        (:nodes document)))

(defn- option-value
  [document option-id]
  (let [attrs (get-in document [:nodes option-id :attrs])]
    (str (if (contains? attrs :value)
           (:value attrs)
           (text-content document option-id)))))

(defn- select-option-ids
  [document select-id]
  (->> (descendant-node-ids document select-id)
       (filter #(= :option (get-in document [:nodes % :tag])))
       vec))

(defn- option-disabled?
  [document option-id]
  (or (truthy-attr? (get-in document [:nodes option-id :attrs :disabled]))
      (loop [parent-id (parent-node-id document option-id)]
        (when parent-id
          (let [parent (get-in document [:nodes parent-id])]
            (if (= :optgroup (:tag parent))
              (truthy-attr? (get-in parent [:attrs :disabled]))
              (recur (parent-node-id document parent-id))))))))

(defn- initialize-select-state
  "Real HTML5 selectedness/`.value`, confirmed against real Chrome before
   touching source (a naive 'disabled never matters' read of the spec
   text is WRONG -- an initial attempt learned this the hard way): an
   EXPLICIT `selected` attr wins outright regardless of `disabled` (a
   `<select>` whose ONLY selected option is `disabled` -- the extremely
   common `<option value=\"x\" disabled selected>` placeholder-with-a-
   real-value idiom -- previously reported `.value` as `\"\"` instead of
   that option's own value, since the old code re-filtered ALL of
   `selected-ids` down to non-disabled options before computing `:value`,
   even when an explicit selection existed). But absent any explicit
   `selected` at all, the DEFAULT falls back to the first NON-disabled
   option, skipping disabled ones entirely (confirmed live: a disabled
   first option with no `selected` anywhere defaults to the next,
   enabled option, not the disabled one) -- and if EVERY option is
   disabled, nothing is selected at all (`selectedIndex -1`, `value \"\"`
   in real Chrome), not a fallback to the plain first option regardless."
  [document select-id]
  (let [options (select-option-ids document select-id)
        multiple? (truthy-attr? (get-in document [:nodes select-id :attrs :multiple]))
        selected-options (filter #(truthy-attr? (get-in document [:nodes % :attrs :selected]))
                                 options)
        selected-ids (if multiple?
                       (vec selected-options)
                       (if-let [selected-id (or (first selected-options)
                                                (first (remove #(option-disabled? document %) options)))]
                         [selected-id]
                         []))
        document (reduce (fn [document option-id]
                           (let [selected? (boolean (some #{option-id} selected-ids))]
                             (-> document
                                 (dom/set-attribute option-id :selected selected?)
                                 (set-attribute-if-missing option-id :default-selected selected?))))
                         document
                         options)]
    (dom/set-attribute document select-id :value
                       (if-let [selected-id (first selected-ids)]
                         (option-value document selected-id)
                         ""))))

(defn- initialize-form-node
  [document id]
  (let [node (get-in document [:nodes id])
        attrs (:attrs node)
        tag (:tag node)
        input-type (str/lower-case (str (or (:type attrs) "")))]
    (case tag
      :input
      (cond-> document
        (not (contains? attrs :default-value))
        (dom/set-attribute id :default-value (str (get attrs :value "")))

        (contains? #{"checkbox" "radio"} input-type)
        (set-attribute-if-missing id :default-checked (truthy-attr? (:checked attrs))))

      :textarea
      (let [value (str (if (contains? attrs :value)
                         (:value attrs)
                         (text-content document id)))]
        (-> document
            (set-attribute-if-missing id :value value)
            (set-attribute-if-missing id :default-value value)))

      :option
      (set-attribute-if-missing document id :default-selected (truthy-attr? (:selected attrs)))

      :select
      (initialize-select-state document id)

      document)))

(defn- initialize-form-defaults
  [document]
  (reduce initialize-form-node
          document
          (sort (keys (:nodes document)))))

(defn- find-top-match-index
  "Index within `stack` (a vector of open element ids, index 0 = root) of the
   nearest (topmost) element whose tag is `tag`, searching from the top of
   the stack downward but never matching the root at index 0. Returns nil if
   no open element with that tag exists (a stray/mismatched closing tag)."
  [document stack tag]
  (loop [i (dec (count stack))]
    (when (pos? i)
      (if (= tag (get-in document [:nodes (nth stack i) :tag]))
        i
        (recur (dec i))))))

(def ^:private auto-close-tags
  "HTML5 elements whose end tag is allowed to be OMITTED because the spec
   defines an implicit close: when a new element starts while a matching
   element still sits at the TOP of the stack of open elements (i.e. it is
   the innermost, most-recently-opened element -- nothing else has opened
   inside it since), the browser implicitly closes that open element first,
   exactly as if a real closing tag for it had appeared, instead of nesting
   the new element inside it. Keys are the tag that may be implicitly
   closed; values are the set of new START tags that trigger the close.

   This is a deliberately SCOPED subset of HTML5's much larger \"omitting
   tags\" table -- only the highest-frequency, most tightly-bounded cases,
   not full spec coverage (see the `:not()`/`:is()`/`:where()`/`:has()`
   scoped-limitation convention in the sibling cssom repo for the shape of
   this kind of documented, honest cut):

     :li     a new <li> closes a currently open <li>. Real HTML5's rule is
             \"may be omitted if the li element is immediately followed by
             another li element, or if there is no more content in its
             parent element\" -- the \"no more content in parent\" half
             needs no extra code here: it already falls out of this
             parser's general behavior of closing every still-open element
             above a matched tag when that ancestor's own end tag arrives
             (see the `:end` case below, and `find-top-match-index`). Only
             the \"immediately followed by another li\" half needs this
             table.
     :option a new <option> OR a new <optgroup> closes a currently open
             <option>. Real HTML5: an <option>'s end tag \"may be omitted
             if the option element is immediately followed by another
             option element, or if it is immediately followed by an
             optgroup element, or if there is no more content in its
             parent element\" -- the \"no more content in parent\" half is
             likewise covered for free by the `:end` case; only the two
             \"immediately followed by\" halves need this table.
     :optgroup a new <optgroup> closes a currently open <optgroup>. Real
             HTML5: an <optgroup>'s end tag \"may be omitted if the
             optgroup element is immediately followed by another optgroup
             element, or if there is no more content in the parent
             element\" -- same shape as <li>/<tr>, \"no more content\"
             covered for free. Combined with :option's own entry above,
             this is the SAME two-level cascading pattern as :td/:th->:tr
             below: a new <optgroup> first closes a currently open
             <option> (exposing the enclosing <optgroup> as the new stack
             top), then that SAME incoming <optgroup> tag is re-examined
             against the now-exposed <optgroup>'s own entry, closing that
             too -- so a grouped <select> with no explicit </option>/
             </optgroup> anywhere (the single most common way authors
             write <optgroup>, and a genuinely severe gap before this
             entry existed: every subsequent <optgroup> nested inside the
             previous option instead of becoming its sibling, confirmed
             by direct reproduction before this fix) gets the correct,
             flat sibling tree shape.
     :p      the real HTML5 rule: a <p>'s end tag may be omitted \"if the p
             element is immediately followed by an address, article, aside,
             blockquote, details, div, dl, fieldset, figcaption, figure,
             footer, form, h1, h2, h3, h4, h5, h6, header, hgroup, hr, main,
             menu, nav, ol, p, pre, section, table, or ul element\" -- the
             full ~29-element block-level list, not just another <p>.
     :dt/:dd definition-list items CROSS-close each other, unlike the
             same-tag-only entries above. Real HTML5: a <dt>'s end tag \"may
             be omitted if the dt element is immediately followed by another
             dt element or a dd element\"; a <dd>'s end tag \"may be omitted
             if the dd element is immediately followed by another dd element
             or a dt element, or if there is no more content in its parent
             element\". So a new <dt> closes a currently open <dt> OR <dd>,
             and a new <dd> closes a currently open <dd> OR <dt> -- hence
             both values below are `#{:dt :dd}`, not a single-tag set. As
             with <li>/<option>, the \"no more content in parent\" half of
             the <dd> rule needs no extra code here: it already falls out of
             the `:end` case closing every still-open descendant above a
             matched ancestor tag. This is the single most common real-world
             definition-list pattern (`<dl><dt>term<dd>def<dt>term<dd>def</dl>`,
             no explicit </dt>/</dd>); nothing broader (e.g. <dt>/<dd> mixed
             with unrelated flow content) is in scope.
     :tr     a new <tr> closes a currently open <tr>. Real HTML5's rule is
             \"may be omitted if the tr element is immediately followed by
             another tr element, or if there is no more content in the
             parent element\" -- same shape as <li>, and the \"no more
             content in parent\" half is likewise free via the `:end` case.
     :td/:th table CELLS cross-close each other, same shape as <dt>/<dd>,
             PLUS a new <tr> also closes a currently open cell -- real
             HTML5: a <td>'s end tag \"may be omitted if the td element is
             immediately followed by a td or th element, __or if there is
             no more content in the parent tr element__\"; a <th>'s end tag
             has the identical shape. This engine has no explicit
             \"no more content in parent\" check the way the `:end` case
             gives that for free elsewhere (a new <tr> is itself a START
             tag, not the parent's end tag) -- so unlike every other entry
             in this table, a new <tr> is included in <td>/<th>'s own
             trigger sets below (`#{:td :th :tr}`) specifically to cover
             that \"no more content\" half via the CASCADING pop this
             table's consumer (`parse-into-document`'s `:start` case) now
             performs: closing the open cell first exposes the enclosing
             <tr> as the new stack top, which <tr>'s OWN entry then also
             matches against the same incoming <tr> tag, closing that too.
             This is the single most common real-world table pattern
             (`<table><tr><td>A<td>B<tr><td>C</table>`, no explicit
             </td>/</tr>) -- a genuinely severe gap before this entry
             existed: without it, every cell after the first nested one
             INSIDE the previous cell, and every row after the first nested
             one inside the previous row's last cell, corrupting the whole
             table's tree shape (confirmed by direct reproduction before
             this fix, including the specific two-level cascading failure
             mode: closing just the cell without also cascading into the
             row left every row after the first still wrongly nested inside
             the previous row's last cell). Implicit <tbody>/<thead>/<tfoot>
             insertion and their own optional end tags are NOT in scope -- a
             real, further gap, but a materially rarer real-world omission
             than bare <tr>/<td>/<th>.

   Deliberately NOT scanned up the stack in a single check: only the TOP of
   the stack is ever examined at each step, matching the \"immediately
   followed by\"/\"most recent\" framing above -- but `parse-into-document`
   now REPEATS that single-top check in a loop, re-examining whatever new
   tag is exposed after each pop, so a chain of matches (like <td>/<th> then
   <tr>) can cascade while a chain that stops matching (like <td>/<th> then
   whatever <tr>'s own PARENT is) correctly halts after the first pop. This
   still matters for correctness, not just simplicity -- e.g. a new <li>
   that opens inside a nested <ul> which itself sits inside an outer,
   still-open <li> (a normal nested list) must NOT reach past the nested
   <ul> to close the outer <li>. It doesn't: the nested <ul> is never a key
   in this table at all, so the loop halts there regardless of how many
   iterations it's allowed. Same reasoning applies to the cross-closing
   <dt>/<dd> and <td>/<th> pairs, and to the new <td>/<th>->cascading-into-
   <tr> chain: a <td> opening inside a NESTED <table> (itself inside an
   outer, still-open <td>/<th> -- a real, if unusual, shape) must not reach
   past the nested <table> to cross-close the outer cell/row. It doesn't,
   for the same reason -- <table> is never a key here, so the loop halts
   the moment the nested <table> is exposed as the new top, regardless of
   how many cascading pops preceded it."
  {:li #{:li}
   :option #{:option :optgroup}
   :optgroup #{:optgroup}
   :p #{:address :article :aside :blockquote :details :div :dl :fieldset
        :figcaption :figure :footer :form :h1 :h2 :h3 :h4 :h5 :h6 :header
        :hgroup :hr :main :menu :nav :ol :p :pre :section :table :ul}
   :dt #{:dt :dd}
   :dd #{:dt :dd}
   :tr #{:tr}
   :td #{:td :th :tr}
   :th #{:td :th :tr}})

(defn- auto-close-stack
  "Repeatedly pops `stack` while the CURRENT top's tag is closeable by the
   incoming `tag-kw` per `auto-close-tags` (see that var's docstring for
   why a single check isn't enough for the <td>/<th>->cascading-into-<tr>
   chain), re-examining whatever new top each pop exposes. Never pops the
   root (index 0). A tag with no cascading partner (li/option/p/dt/dd)
   still only ever pops once, since none of THEIR parents are themselves
   keys in `auto-close-tags`."
  [document stack tag-kw]
  (loop [stack stack]
    (if (and (> (count stack) 1)
             (contains? (auto-close-tags (get-in document [:nodes (peek stack) :tag])) tag-kw))
      (recur (pop stack))
      stack)))

(defn- maybe-insert-implicit-tbody
  "Real HTML5's \"in table\" insertion mode inserts an implicit <tbody>
   before a <tr> start tag when no <tbody>/<thead>/<tfoot> is already open
   -- i.e. the single most common real-world table shape, rows with no
   explicit row-group wrapper at all (`<table><tr>...</tr><tr>...</tr>
   </table>`). Without this, every <tr> nested directly under <table>,
   which is a genuinely visible, reachable bug, not just a DOM-purity
   nicety: a `table > tr` CSS child-combinator selector wrongly MATCHED
   (confirmed by direct reproduction), when real HTML5/CSS never lets it
   -- a real tr's parent is always a row group -- while the equally common
   `tbody > tr` selector wrongly never matched a bare table's own rows.

   Only triggers when the current stack top is `:table` itself (no
   row-group open yet); a second, third, etc. <tr> correctly reuses the
   SAME already-open implicit <tbody> instead of inserting another one,
   since `auto-close-stack` (called before this, on `:tr`'s own trigger
   set `#{:tr}`) pops the previous <tr> (and any still-open <td>/<th>)
   but deliberately does NOT pop the synthesized <tbody> itself -- `:tbody`
   is not a key in `auto-close-tags` -- so it stays as the new stack top.

   Deliberately scoped to `:tr` only, mirroring the same \"most common
   case, not full spec coverage\" convention as `auto-close-tags`: a bare
   <td>/<th> directly under <table> with no <tr> at all (itself already
   an unusual, arguably-malformed shape) is NOT covered here -- it still
   nests directly under whatever the current stack top is, same as before
   this fix. Explicit <thead>/<tbody>/<tfoot> tags are unaffected either
   way -- this only ever fires when none of the three appear in the
   source at all."
  [document stack tag-kw]
  (if (and (= tag-kw :tr)
           (= :table (get-in document [:nodes (peek stack) :tag])))
    (let [[tbody-id document] (dom/create-element document :tbody)
          document (dom/append-child document (peek stack) tbody-id)]
      [document (conj stack tbody-id)])
    [document stack]))

(defn- preserve-whitespace-context?
  "Whether the CURRENT stack means whitespace in an about-to-be-created
   text node must be preserved verbatim rather than collapsed to single
   spaces: either (a) the innermost open element is a raw-text/RCDATA tag
   (`<script>`/`<style>`/`<title>`/`<textarea>`) -- their own text content
   always arrives here as a single, tag-less `:text` token via
   `tokenize`'s separate raw-text-scanning branch (see `raw-text-tags`),
   and was ALREADY verbatim before whitespace collapsing moved into this
   fn, so it must stay that way -- or (b) a real `<pre>` is anywhere in
   the stack (not just the immediate parent: a `<span>`/`<code>` nested
   inside a `<pre>` still needs its OWN text preserved, since `<pre>`,
   unlike raw-text tags, allows real nested markup)."
  [document stack]
  (or (contains? raw-text-tags (name (get-in document [:nodes (peek stack) :tag])))
      (some #(= :pre (get-in document [:nodes % :tag])) stack)))

;; ============================================================
;; Active formatting elements + adoption agency (WHATWG §13.2.4.3, §13.2.6.4.7)
;;
;; L2: the real WHATWG algorithms for mis-nested formatting elements, layered
;; on the existing subset tokenizer + auto-closing tree builder. They operate
;; within the engine's :document-root document model -- NO html/head/body
;; synthesis (that is an L3 architectural change requiring downstream
;; coordination in cssom/layout/dom-bridge, which all assume content nests
;; directly under :document). They activate only on mis-nested input;
;; well-formed HTML parses byte-identically to before, so the existing 133-
;; test suite is a regression gate.
;;
;; In scope (L2): the list of active formatting elements with the Noah's Ark
;; clause; reconstruct active formatting elements; the adoption agency
;; algorithm's no-furthest-block path (steps 1-8 + the any-other-end-tag
;; fallback), which correctly handles the common mis-nested formatting shapes
;; real browsers fix (<b>p<i>s</b>e</i> -> <b>p<i>s</i></b><i>e</i>;
;; <b>x<i>y</b>z -> <b>x<i>y</i></b><i>z</i>; <div><b>x</div>y reopens b).
;;
;; Deliberately deferred to L3 (documented honest cuts, same convention as
;; `auto-close-tags`): the adoption agency's furthest-block reparenting path
;; (steps 9-19 -- rare block-in-inline like <b><p>x</b>, falls back to naive
;; nesting here, no crash); foster parenting (in-table); the full 23-mode
;; insertion-mode state machine; html/head/body synthesis; template/select/
;; frameset modes; scope markers (cells are not real insertion modes yet, so
;; formatting may leak across table cells); the `a`/`nobr` in-scope special
;; cases; "generate implied end tags" for the any-other-end-tag path.

(def ^:private formatting-tags
  "WHATWG §13.2.4.2 'Formatting' category -- start tags that push onto the
  list of active formatting elements; end tags that run the adoption agency
  algorithm."
  #{:a :b :big :code :em :font :i :nobr :s :small :strike :strong :tt :u})

(def ^:private scope-tags
  "Default scope boundaries (WHATWG §13.2.4.2 'has an element in scope'), HTML
  namespace only. The :document root (stack index 0) acts as the scope ceiling
  in lieu of an html element."
  #{:applet :caption :html :table :td :th :marquee :object :select :template})

(defn- tag-of [document id] (get-in document [:nodes id :tag]))

(defn- push-afe
  "Push `id` onto the afe list with the Noah's Ark clause (WHATWG §13.2.4.3):
  if there are already three entries with the same tag, remove the earliest
  before adding. (No markers in L2 -- see namespace commentary -- so the
  whole list is one segment.)"
  [document afe id]
  (let [tag (tag-of document id)
        same-tag (filter #(= (tag-of document %) tag) afe)]
    (if (>= (count same-tag) 3)
      (conj (vec (remove #(= % (first same-tag)) afe)) id)
      (conj afe id))))

(defn- in-scope?
  "WHATWG 'has an element in scope' (default scope): scan the stack from the
  current node (top) toward the root; match if `target-tag` is found before
  any scope boundary. The :document root (index 0) is the scope ceiling."
  [document stack target-tag]
  (loop [i (dec (count stack))]
    (when (>= i 0)
      (let [t (tag-of document (nth stack i))]
        (cond
          (= t target-tag) true
          (= i 0) false
          (contains? scope-tags t) false
          :else (recur (dec i)))))))

(defn- insert-element-for-token
  "Create a new element with `src-id`'s tag and attrs ('an element for the
  token for which src-id was created'), append it to `parent-id`, return
  [new-id document]. Used by reconstruct-active-formatting."
  [document parent-id src-id]
  (let [tag (tag-of document src-id)
        [new-id document] (dom/create-element document tag)
        document (dom/append-child document parent-id new-id)
        document (apply-attrs document new-id (get-in document [:nodes src-id :attrs]))]
    [new-id document]))

(defn- reconstruct-active-formatting
  "WHATWG §13.2.4.3. Reopens formatting elements that are in the afe list but
  no longer on the stack of open elements (they were implicitly closed by an
  end tag for an ancestor), so subsequent text/elements inherit the active
  formatting. No-op when the list is empty or the last entry is already on
  the stack. Returns [document stack afe]."
  [document stack afe]
  (let [v (vec afe)
        n (count v)]
    (if (or (zero? n)
            (some #(= % (peek v)) stack))
      [document stack afe]
      (let [in-stack? (fn [e] (some #(= % e) stack))
            ;; rewind: largest index < n-1 whose entry is in the stack; entries
            ;; after it need reconstruction. -1 if none (reconstruct from 0).
            boundary (loop [i (- n 2)]
                       (cond
                         (< i 0) -1
                         (in-stack? (nth v i)) i
                         :else (recur (dec i))))
            from (inc boundary)]
        (loop [document document stack stack v v i from]
          (if (>= i n)
            [document stack v]
            (let [old-id (nth v i)
                  [new-id document] (insert-element-for-token document (peek stack) old-id)]
              (recur document (conj stack new-id) (assoc v i new-id) (inc i)))))))))

(defn- adoption-agency
  "WHATWG §13.2.6.4.7 adoption agency algorithm for an end tag whose tag name
  is `subject` (a formatting element). Handles mis-nested formatting.

  L2 implements steps 1-8 (the no-furthest-block path) + the any-other-end-tag
  fallback (step 4.3): this correctly handles every case where no special
  (block) element sits between the formatting element and the current node --
  i.e. the common real-world mis-nesting. When a furthest block IS present
  (rare block-in-inline like <b><p>x</b>), L2 falls back to the no-furthest-
  block behavior (pop to the formatting element, remove it from the afe list)
  rather than the full reparenting path (steps 9-19, deferred to L3): the
  result is naive L1 nesting for that rare shape, never a crash. (The full
  AAA's 8-iteration outer loop is unnecessary here because both L2 paths
  return after a single pass; it returns with the reparenting path at L3.)
  Returns [document stack afe]."
  [document stack afe subject]
  (let [stack (vec stack)
        afe (vec afe)
        current (peek stack)]
    (if (and (= (tag-of document current) subject)
             (not (some #(= % current) afe)))
      ;; step 2: current node is subject and not in afe -> pop, return.
      [document (pop stack) afe]
      (let [;; step 4.3: formattingElement = last afe entry with tag subject
            fmt (last (filter #(= (tag-of document %) subject) afe))]
        (if (nil? fmt)
          ;; no formatting element -> any-other-end-tag: pop to subject.
          (let [match-idx (find-top-match-index document stack subject)]
            [document (if match-idx (subvec stack 0 match-idx) stack) afe])
          (let [fmt-idx (.indexOf stack fmt)]
            (cond
              ;; step 4: fmt not in stack -> remove from afe, return.
              (neg? fmt-idx)
              [document stack (vec (remove #(= % fmt) afe))]
              ;; step 5: fmt not in scope -> return.
              (not (in-scope? document stack subject))
              [document stack afe]
              ;; L2: step 7's furthestBlock search + step 8 (no furthest block)
              ;; and the furthest-block fallback (full reparenting steps 9-19,
              ;; deferred to L3) all collapse to the same action here -- pop to
              ;; fmt and remove fmt from the afe list.
              :else
              [document (subvec stack 0 fmt-idx)
               (vec (remove #(= % fmt) afe))])))))))

(defn parse-into-document
  [html]
  (let [[root-id document] (dom/create-element dom/empty-document :document)
        document (dom/set-root document root-id)]
    (loop [document document
           stack [root-id]
           afe []
           tokens (seq (tokenize html))]
      (if-let [{:keys [type tag attrs self? text]} (first tokens)]
        (case type
          :text
          (let [text (if (preserve-whitespace-context? document stack)
                       text
                       ;; Collapse runs of spaces/tabs, but KEEP newlines:
                       ;; whether a newline is a line break or just another
                       ;; space is a CSS decision (`white-space: pre-line`
                       ;; breaks on it, `normal` collapses it), and a parser
                       ;; cannot see CSS. Destroying newlines here made
                       ;; `white-space: pre-wrap`/`pre-line` impossible to
                       ;; implement at all -- the information was gone
                       ;; before layout ever ran. cssom.layout collapses
                       ;; them for `normal`/`nowrap`, which is where that
                       ;; decision belongs.
                       ;;
                       ;; The character class is spelled out rather than
                       ;; written as `[^\S\n]` (everything `\s` matches
                       ;; except newline), because `\s` ALSO matches U+00A0
                       ;; NO-BREAK SPACE -- so `&nbsp;`, having just been
                       ;; correctly decoded to U+00A0 by `decode-entities`,
                       ;; was immediately rewritten back to an ordinary
                       ;; U+0020 and became collapsible again. That destroys
                       ;; the one thing `&nbsp;` exists to do, and CSS says
                       ;; so explicitly: only spaces, tabs and segment breaks
                       ;; are collapsible white space; U+00A0 is not.
                       ;; Measured against a real browser by the conformance
                       ;; harness (case `entity/nbsp-is-not-collapsible`):
                       ;; `<p>a&nbsp;b</p>` gave the browser codepoints
                       ;; 97/160/98 and this parser 97/32/98. The class below
                       ;; is exactly HTML's own "ASCII whitespace" set minus
                       ;; the newline this deliberately keeps.
                       (str/replace text #"[ \t\f\r]+" " "))
                [document stack afe] (reconstruct-active-formatting document stack afe)
                [id document] (dom/create-text-node document text)]
            (recur (dom/append-child document (peek stack) id) stack afe (next tokens)))

          :start
          (let [tag-kw (keyword tag)
                ;; Optional-end-tag auto-closing (see `auto-close-tags`/
                ;; `auto-close-stack`): if the innermost currently-open
                ;; element (top of the stack) is one this new start tag
                ;; implicitly closes -- e.g. a new <li> while an <li> is
                ;; already open -- pop it first, as if a real closing tag
                ;; for it had just appeared, cascading through further pops
                ;; when the newly-exposed top ALSO matches (e.g. a new <tr>
                ;; closing an open <td> then also the enclosing <tr>).
                stack (auto-close-stack document stack tag-kw)
                [document stack] (maybe-insert-implicit-tbody document stack tag-kw)
                ;; Reconstruct active formatting elements before inserting any
                ;; new element (WHATWG in-body), so mis-nested formatting that
                ;; was implicitly closed reopens around the new element.
                [document stack afe] (reconstruct-active-formatting document stack afe)
                [id document] (dom/create-element document tag)
                document (-> document
                             (apply-attrs id attrs)
                             (dom/append-child (peek stack) id))
                afe (if (contains? formatting-tags tag-kw)
                      (push-afe document afe id)
                      afe)
                remaining (next tokens)
                ;; WHATWG spec: if a <pre>'s content begins with a single
                ;; U+000A LINE FEED, that one character is silently dropped
                ;; -- the same authoring-convenience rule textarea gets
                ;; above, but <pre> allows real nested markup so it can't be
                ;; handled at tokenize time; only the token IMMEDIATELY
                ;; following <pre>'s own start tag is eligible (a nested
                ;; child's own leading newline, e.g. <pre><span>\nx</span>
                ;; </pre>, is untouched -- that <span>'s first token here is
                ;; a :start, not :text, so this check simply doesn't match).
                remaining (if (and (= :pre tag-kw) (not self?)
                                   (= :text (:type (first remaining)))
                                   (str/starts-with? (:text (first remaining)) "\n"))
                            (let [stripped (subs (:text (first remaining)) 1)]
                              (if (seq stripped)
                                (cons (assoc (first remaining) :text stripped) (next remaining))
                                (next remaining)))
                            remaining)]
            (recur document (if self? stack (conj stack id)) afe remaining))

          :end
          (let [tag-kw (keyword tag)]
            (if (contains? formatting-tags tag-kw)
              ;; Formatting end tag -> adoption agency algorithm.
              (let [[document stack afe] (adoption-agency document stack afe tag-kw)]
                (recur document stack afe (next tokens)))
              ;; Any other end tag -> pop to the nearest matching open element.
              (let [match-idx (find-top-match-index document stack tag-kw)]
                (recur document
                       (if match-idx (subvec stack 0 match-idx) stack)
                       afe
                       (next tokens)))))

          (recur document stack afe (next tokens)))
        (initialize-form-defaults document)))))
