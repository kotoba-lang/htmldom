(ns htmldom.adoption-agency-test
  "L2 adoption-agency / active-formatting tests. These are the mis-nested
  formatting shapes the L1 subset tokenizer got wrong and a real WHATWG
  parser fixes via the list of active formatting elements + reconstruct +
  the adoption agency algorithm. Each expected tree was verified against
  real browser behavior (html5lib / Chrome / Firefox) before encoding."
  (:require [clojure.test :refer [deftest is]]
            [htmldom.core :as html]
            [kotoba.wasm.dom :as dom]))

(defn- tree [html] (dom/tree (html/parse-into-document html)))

(defn- shape-children
  "Render a node list as a nested vector of `[tag & children]` (elements) or
  raw strings (text nodes), for compact structural assertion."
  [nodes]
  (mapv (fn [n] (if (map? n) (into [(:tag n)] (shape-children (:children n))) n)) nodes))

(defn- shape
  "Render a parsed document's root children as a nested shape vector."
  [html]
  (shape-children (:children (tree html))))

(deftest adoption-agency-b-p-i-s-b-e-i
  ;; The canonical html5lib case: <b>p<i>s</b>e</i> -> <b>p<i>s</i></b><i>e</i>
  ;; The <i> opened inside <b> is adopted into <b> before </b> closes it, then
  ;; <i> is reopened for "e" as a sibling of <b>.
  (is (= [[:b "p" [:i "s"]] [:i "e"]]
         (shape "<b>p<i>s</b>e</i>"))))

(deftest adoption-agency-b-x-i-y-b-z
  ;; <b>x<i>y</b>z -> <b>x<i>y</i></b><i>z</i>. Same shape as above; "z" lands
  ;; in a reopened <i>, NOT nested under a reopened <b> (b is removed from the
  ;; active formatting list by the adoption agency, so only <i> reopens).
  (is (= [[:b "x" [:i "y"]] [:i "z"]]
         (shape "<b>x<i>y</b>z"))))

(deftest adoption-agency-b-1-i-2-b-3-i
  ;; <b>1<i>2</b>3</i> -> <b>1<i>2</i></b><i>3</i>.
  (is (= [[:b "1" [:i "2"]] [:i "3"]]
         (shape "<b>1<i>2</b>3</i>"))))

(deftest end-tag-for-ancestor-reopens-formatting
  ;; <div><b>x</div>y -> <div><b>x</b></div><b>y</b>. </div> implicitly closes
  ;; the still-open <b>, but <b> stays in the active formatting list, so it is
  ;; reconstructed (reopened) for "y" as a sibling of the <div> -- exactly
  ;; what real browsers do.
  (is (= [[:div [:b "x"]] [:b "y"]]
         (shape "<div><b>x</div>y"))))

(deftest properly-closed-formatting-is-not-reopened
  ;; <b>x</b>y -> <b>x</b>y. A formatting element closed by its OWN end tag is
  ;; removed from the active formatting list, so it is NOT reopened for later
  ;; text. (Regression guard: reconstruct must be a no-op when the last afe
  ;; entry is on the stack, and AAA must remove fmt from the afe list.)
  (is (= [[:b "x"] "y"]
         (shape "<b>x</b>y"))))

(deftest reconstruct-preserves-formatting-attributes
  ;; A reconstructed (reopened) formatting element keeps the original's
  ;; attributes -- "an element for the token for which the entry was created".
  ;; <div><b class="x">y</div>z -> <div><b class=x>y</b></div><b class=x>z</b>
  (let [children (:children (tree "<div><b class=\"x\">y</div>z"))]
    (is (= [[:div [:b "y"]] [:b "z"]] (shape-children children)))
    (is (= "x" (get-in (second children) [:attrs :class])))))

(deftest reconstruct-stacks-multiple-open-formatting-elements
  ;; Two open formatting elements both reopen in order: <b><i>a</b>cd</i> ->
  ;; <b><i>a</i></b><i>cd</i>. After </b>, <i> (still in the afe list) is
  ;; reconstructed for "cd".
  (is (= [[:b [:i "a"]] [:i "cd"]]
         (shape "<b><i>a</b>cd</i>"))))

(deftest furthest-block-fallback-is-naive-nesting
  ;; L2 documented limitation, locked here so a future L3 reparenting
  ;; implementation is an intentional change, not a silent one. <b><p>x</b>
  ;; (a special/block element -- <p> -- between the formatting element <b>
  ;; and the current node, i.e. a furthest block is present) does NOT take
  ;; the full adoption-agency reparenting path (steps 9-19, deferred to L3);
  ;; it falls back to popping to <b>, leaving naive L1 nesting. A real
  ;; browser produces <b></b><p><b>x</b></p>; L3 will close that gap.
  (is (= [[:b [:p "x"]]]
         (shape "<b><p>x</b>"))))
