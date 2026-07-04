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

(defn- attrs
  [s]
  (->> (re-seq #"([A-Za-z_:][-A-Za-z0-9_:.]*)(?:\s*=\s*(?:\"([^\"]*)\"|'([^']*)'|([^\s\"'>/]+)))?" s)
       (map (fn [[_ k dq sq bare]]
              [(keyword (str/lower-case k)) (or dq sq bare true)]))
       (into {})))

(defn- token
  [raw]
  (if (str/starts-with? raw "<")
    (let [body (subs raw 1 (dec (count raw)))
          closing? (str/starts-with? body "/")
          body (str/trim (if closing? (subs body 1) body))
          self? (or (str/ends-with? body "/")
                    (contains? void-tags (str/lower-case (first (str/split body #"\s+" 2)))))
          body (str/trim (if (str/ends-with? body "/") (subs body 0 (dec (count body))) body))
          [tag attr-text] (str/split body #"\s+" 2)
          tag (str/lower-case (or tag ""))]
      (if closing?
        {:type :end :tag tag}
        {:type :start :tag tag :attrs (attrs (or attr-text "")) :self? self?}))
    (let [text (str/replace raw #"\s+" " ")]
      (when-not (str/blank? text)
        {:type :text :text text}))))

(def ^:private raw-text-tags
  "Elements whose content real HTML parsers scan as literal raw text rather
   than markup: `<`/`>`/quotes inside them are not interpreted, and only the
   first matching end tag terminates them.

   `script`/`style` are true \"raw text\" elements per the WHATWG spec.
   `title`/`textarea` are technically \"RCDATA\" -- the one difference being
   that character references (e.g. `&amp;`) are still decoded inside RCDATA.
   This project does not do HTML entity decoding anywhere yet (a separate,
   out-of-scope gap), so that distinction is a no-op here: all four tags get
   identical literal-scan treatment, which matches real-parser behavior for
   everything this parser currently does."
  #{"script" "style" "title" "textarea"})

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
          (let [[id document] (dom/create-element document tag)
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
