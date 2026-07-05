(ns htmldom.core
  "Small trusted HTML subset parser for the kotoba-only browser R0.

   This intentionally does not claim WHATWG HTML compatibility. It only bridges
   simple HTML-like documents into kotoba.wasm.dom.

   Split out of kotoba-lang/browser (ADR-2607051140). Not to be confused with
   kotoba-lang/html, an unrelated Hiccup-compatible EDN HTML renderer (data ->
   HTML text); this namespace goes the other direction: parses HTML text into
   a kotoba.wasm.dom document."
  (:require [clojure.string :as str]
            [kotoba.wasm.dom :as dom]))

(def void-tags
  #{"area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta"
    "param" "source" "track" "wbr"})

(defn- parse-style-value
  [v]
  (let [v (str/trim v)]
    (cond
      (re-matches #"-?\d+" v) #?(:clj (Long/parseLong v) :cljs (js/parseInt v 10))
      (re-matches #"-?\d+px" v) #?(:clj (Long/parseLong (subs v 0 (- (count v) 2)))
                                   :cljs (js/parseInt v 10))
      :else v)))

(defn parse-style
  [style-text]
  (->> (str/split (or style-text "") #";")
       (keep (fn [decl]
               (let [[k v] (map str/trim (str/split decl #":" 2))]
                 (when (and (seq k) (seq v))
                   [(keyword k) (parse-style-value v)]))))
       (into {})))

(def ^:private named-char-refs
  "A small, pragmatic subset of HTML5 named character references: the five
   XML-predefined entities (universally supported, unambiguous, no lookup
   table strictly needed) plus a handful of the other named entities most
   likely to show up in real-world text content. This is deliberately NOT the
   full ~2000-entry HTML5 named character reference table -- implementing
   that would be wildly out of scope for a trusted-HTML-subset parser. Any
   named entity not in this table is left as literal text (`&foo;` stays
   `&foo;`) rather than guessed at, matching this project's existing
   degrade-don't-guess convention (see `parse-style-value`'s handling of
   unsupported CSS values)."
  {"amp" "&" "lt" "<" "gt" ">" "quot" "\"" "apos" "'"
   "nbsp" "\u00A0" "copy" "\u00A9" "reg" "\u00AE" "trade" "\u2122"
   "mdash" "\u2014" "ndash" "\u2013" "hellip" "\u2026"})

(defn- codepoint->str
  "A Unicode codepoint (possibly outside the BMP, e.g. an emoji) as a string,
   correctly emitted as a surrogate pair where needed."
  [cp]
  #?(:clj (String. (Character/toChars (int cp)))
     :cljs (js/String.fromCodePoint cp)))

(defn- numeric-char-ref->str
  "Decode the digits of a numeric character reference (`radix` 10 or 16) to
   the corresponding Unicode codepoint's string, or nil if the digits don't
   parse or name a codepoint outside the valid Unicode range (including the
   surrogate-only range, which is never a valid scalar value) -- callers fall
   back to leaving the reference as literal text, same as an unrecognized
   named entity."
  [digits radix]
  (try
    (let [cp #?(:clj (Long/parseLong digits radix) :cljs (js/parseInt digits radix))]
      (when (and (<= 0 cp 0x10FFFF) (not (<= 0xD800 cp 0xDFFF)))
        (codepoint->str cp)))
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
                 true)]))
       (into {})))

(defn- token
  [raw]
  (if (str/starts-with? raw "<")
    (let [body (subs raw 1 (dec (count raw)))
          closing? (str/starts-with? body "/")
          body (str/trim (if closing? (subs body 1) body))
          ;; A trailing `/` right before the tag's `>` is always stripped from
          ;; the tag/attribute text (matching real HTML5, which ignores a
          ;; stray trailing slash on any start tag) -- but this must happen
          ;; BEFORE the tag name is extracted, and self-closing status is
          ;; determined SOLELY by void-tag membership, not by the presence of
          ;; this slash: real HTML5 only treats actual void elements
          ;; (`<br/>`, `<img/>`, ...) as self-closing. A trailing `/` on an
          ;; ordinary element like `<div/>` is ignored -- the element still
          ;; needs, and will be matched against, a real closing tag later.
          body (str/trim (if (str/ends-with? body "/") (subs body 0 (dec (count body))) body))
          [tag attr-text] (str/split body #"\s+" 2)
          tag (str/lower-case (or tag ""))
          self? (contains? void-tags tag)]
      (if closing?
        {:type :end :tag tag}
        {:type :start :tag tag :attrs (attrs (or attr-text "")) :self? self?}))
    (let [text (str/replace raw #"\s+" " ")]
      (when-not (str/blank? text)
        {:type :text :text (decode-entities text)}))))

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

(defn tokenize
  [html]
  (let [html (or html "")
        len (count html)]
    (loop [pos 0
           acc []]
      (let [lt (or (str/index-of html "<" pos) len)
            acc (if (> lt pos)
                  (if-let [t (token (subs html pos lt))]
                    (conj acc t)
                    acc)
                  acc)]
        (cond
          (>= lt len)
          acc

          ;; HTML comment: scanned literally for its real `-->` terminator so
          ;; a `>` inside the comment body can't truncate it and corrupt the
          ;; rest of the token stream. Comments produce no token (discarded),
          ;; matching prior behavior.
          (str/starts-with? (subs html lt) "<!--")
          (let [end (str/index-of html "-->" (+ lt 4))]
            (recur (if end (+ end 3) len) acc))

          :else
          (let [gt (str/index-of html ">" lt)]
            (if (nil? gt)
              ;; No terminating '>' anywhere in the rest of the input: drop
              ;; this lone '<' (matches the old regex tokenizer's lenient
              ;; behavior of silently skipping an unmatched '<') and continue.
              (recur (inc lt) acc)
              (let [raw (subs html lt (inc gt))]
                (if (or (str/starts-with? raw "<!") (str/starts-with? raw "<?"))
                  ;; doctype / processing instruction: discarded, as before.
                  (recur (inc gt) acc)
                  (let [raw-tag (raw-text-tag-name html lt gt)
                        open-tok (token raw)]
                    (if (and raw-tag (not (:self? open-tok)))
                      ;; Raw-text element: emit its real opening tag, then the
                      ;; verbatim (whitespace-preserving) text up to the real
                      ;; closing tag, then the real closing tag itself.
                      (let [close-idx (raw-text-close-index html raw-tag (inc gt))
                            text (subs html (inc gt) close-idx)
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
                          (let [close-gt (or (str/index-of html ">" close-idx) len)]
                            (recur (if (< close-gt len) (inc close-gt) len)
                                   (conj acc {:type :end :tag raw-tag})))
                          (recur len acc)))
                      (recur (inc gt) (if open-tok (conj acc open-tok) acc)))))))))))))

(defn- apply-attrs
  [document id attrs]
  (reduce-kv
   (fn [document k v]
     (if (= k :style)
       (let [style (parse-style v)]
         (-> document
             (dom/set-attribute id :style-inline style)
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
  [document select-id]
  (let [options (select-option-ids document select-id)
        multiple? (truthy-attr? (get-in document [:nodes select-id :attrs :multiple]))
        selected-options (filter #(truthy-attr? (get-in document [:nodes % :attrs :selected]))
                                 options)
        selected-ids (if multiple?
                       (vec selected-options)
                       (if-let [selected-id (or (first selected-options)
                                                (first options))]
                         [selected-id]
                         []))
        first-enabled-selected (first (remove #(option-disabled? document %) selected-ids))
        document (reduce (fn [document option-id]
                           (let [selected? (boolean (some #{option-id} selected-ids))]
                             (-> document
                                 (dom/set-attribute option-id :selected selected?)
                                 (set-attribute-if-missing option-id :default-selected selected?))))
                         document
                         options)]
    (dom/set-attribute document select-id :value
                       (if first-enabled-selected
                         (option-value document first-enabled-selected)
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
     :option a new <option> closes a currently open <option>. Same shape as
             <li>: the spec also lets end-of-parent (or a following
             <optgroup>) close an <option>, which end-of-parent is likewise
             covered for free by the `:end` case; a following <optgroup> is
             out of scope.
     :p      a new <p> closes a currently open <p>. Real HTML5's actual <p>
             rule is much broader (a specific list of ~30 block-level
             elements -- <div>, <ul>, <h1>, etc. -- also implicitly close an
             open <p>, not just another <p>). That fuller rule is
             deliberately OUT OF SCOPE here: this engine only auto-closes an
             open <p> when another <p> starts, which is the single most
             common real-world case (`<p>one<p>two`). A <p> immediately
             followed by an unrelated block element still (wrongly, but
             boundedly, and no worse than before this table existed) nests
             under this parser.
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

   Deliberately NOT scanned up the stack: only the top of the stack (the
   innermost currently-open element) is ever checked, matching the
   \"immediately followed by\"/\"most recent\" framing above. This matters
   for correctness, not just simplicity -- e.g. a new <li> that opens inside
   a nested <ul> which itself sits inside an outer, still-open <li> (a
   normal nested list) must NOT reach past the nested <ul> to close the
   outer <li>. It doesn't: the nested <ul> occupies the top of the stack at
   that point, not the outer <li>, so the outer <li> is never even
   examined. Same reasoning applies to the cross-closing <dt>/<dd> pair: a
   <dt> opening inside a nested <dl> (itself inside an outer, still-open
   <dd> or <dt>) must NOT reach past the nested <dl> to cross-close the
   outer <dt>/<dd> it doesn't actually follow. It doesn't, for the same
   reason -- the nested <dl> occupies the top of the stack, not the outer
   <dt>/<dd>."
  {:li #{:li}
   :option #{:option}
   :p #{:p}
   :dt #{:dt :dd}
   :dd #{:dt :dd}})

(defn parse-into-document
  [html]
  (let [[root-id document] (dom/create-element dom/empty-document :document)
        document (dom/set-root document root-id)]
    (loop [document document
           stack [root-id]
           tokens (seq (tokenize html))]
      (if-let [{:keys [type tag attrs self? text]} (first tokens)]
        (case type
          :text
          (let [[id document] (dom/create-text-node document text)]
            (recur (dom/append-child document (peek stack) id) stack (next tokens)))

          :start
          (let [tag-kw (keyword tag)
                ;; Optional-end-tag auto-closing (see `auto-close-tags`): if
                ;; the innermost currently-open element (top of the stack)
                ;; is one this new start tag implicitly closes -- e.g. a new
                ;; <li> while an <li> is already open -- pop it first, as if
                ;; a real closing tag for it had just appeared. Never pops
                ;; the root (index 0).
                top-tag (get-in document [:nodes (peek stack) :tag])
                stack (if (and (> (count stack) 1)
                               (contains? (auto-close-tags top-tag) tag-kw))
                        (pop stack)
                        stack)
                [id document] (dom/create-element document tag)
                document (-> document
                             (apply-attrs id attrs)
                             (dom/append-child (peek stack) id))]
            (recur document (if self? stack (conj stack id)) (next tokens)))

          :end
          (let [match-idx (find-top-match-index document stack (keyword tag))]
            (recur document
                   (if match-idx (subvec stack 0 match-idx) stack)
                   (next tokens)))

          (recur document stack (next tokens)))
        (initialize-form-defaults document)))))
