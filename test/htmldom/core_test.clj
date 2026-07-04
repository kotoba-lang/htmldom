(ns htmldom.core-test
  (:require [clojure.test :refer [deftest is]]
            [htmldom.core :as html]
            [kotoba.wasm.dom :as dom]))

(deftest parses-simple-element-tree
  (let [document (html/parse-into-document "<main id=\"app\"><p class=\"lead\">Hi</p></main>")
        root (dom/node document (:root document))
        main (dom/node document (first (:children root)))
        p (dom/node document (first (:children main)))]
    (is (= :main (:tag main)))
    (is (= "app" (get-in main [:attrs :id])))
    (is (= :p (:tag p)))
    (is (= "lead" (get-in p [:attrs :class])))
    (is (= "Hi" (dom/text-content document)))))

(deftest void-tags-do-not-consume-a-closing-tag
  (let [document (html/parse-into-document "<main><br><img src=\"x.png\">tail</main>")
        root (dom/node document (:root document))
        main (dom/node document (first (:children root)))
        tags (mapv #(:tag (dom/node document %)) (:children main))]
    (is (= [:br :img nil] tags))))

(deftest inline-style-is-parsed-into-style-attrs
  (let [document (html/parse-into-document "<main><p style=\"color: red; padding: 4px\">Hi</p></main>")
        root (dom/node document (:root document))
        main (dom/node document (first (:children root)))
        p (dom/node document (first (:children main)))]
    (is (= "red" (get-in p [:attrs :style/color])))
    (is (= 4 (get-in p [:attrs :style/padding])))))

(deftest select-initializes-value-from-selected-option
  (let [document (html/parse-into-document
                  "<select><option value=\"a\">A</option><option value=\"b\" selected>B</option></select>")
        root (dom/node document (:root document))
        select (dom/node document (first (:children root)))]
    (is (= "b" (get-in select [:attrs :value])))))

(deftest script-content-is-raw-text-not-markup
  ;; A `<script>` body containing `<`/`>`/quotes must not be interpreted as
  ;; markup: the browser scans it literally up to the real `</script>`.
  (let [document (html/parse-into-document
                  "<script>if (1 < 2) { x(); }</script><p>after</p>")
        tree (dom/tree document)
        [script p] (:children tree)]
    (is (= :script (:tag script)))
    (is (= ["if (1 < 2) { x(); }"] (:children script)))
    (is (= :p (:tag p)))
    (is (= ["after"] (:children p)))))

(deftest script-content-with-embedded-quotes-and-fake-closing-tag
  ;; A JS string literal that spells out a fake `</script...>` must not
  ;; terminate the element early; only a genuine end-tag boundary does.
  (let [document (html/parse-into-document
                  "<script>var s = \"</scriptTAG>\"; if (a<b && c>d) {}</script><p>after</p>")
        tree (dom/tree document)
        [script p] (:children tree)]
    (is (= :script (:tag script)))
    (is (= ["var s = \"</scriptTAG>\"; if (a<b && c>d) {}"] (:children script)))
    (is (= :p (:tag p)))))

(deftest style-content-is-raw-text-not-markup
  ;; Same raw-text handling applies to `<style>`, and whitespace inside it
  ;; must be preserved verbatim (unlike ordinary text nodes).
  (let [document (html/parse-into-document
                  "<style>.a[data-x=\">\"]  {  color: red;  }</style><p>after</p>")
        tree (dom/tree document)
        [style p] (:children tree)]
    (is (= :style (:tag style)))
    (is (= [".a[data-x=\">\"]  {  color: red;  }"] (:children style)))
    (is (= :p (:tag p)))))

(deftest comment-containing-greater-than-does-not-corrupt-parsing
  ;; `>` inside an HTML comment must not truncate the comment; the comment is
  ;; discarded (no node) and parsing resumes correctly after its real `-->`.
  (let [document (html/parse-into-document
                  "<p>before</p><!-- if (a>b) then... --><p>after</p>")
        tree (dom/tree document)
        [p1 p2] (:children tree)]
    (is (= 2 (count (:children tree))))
    (is (= :p (:tag p1)))
    (is (= ["before"] (:children p1)))
    (is (= :p (:tag p2)))
    (is (= ["after"] (:children p2)))))

(deftest back-to-back-comments-do-not-corrupt-parsing
  (let [document (html/parse-into-document "<!-- x --><!-- y --><p>after</p>")
        tree (dom/tree document)]
    (is (= 1 (count (:children tree))))
    (is (= :p (:tag (first (:children tree)))))))

(deftest mismatched-closing-tag-recovers-by-closing-intervening-elements
  ;; `<div><span>text</div>` -- the unclosed `<span>` must be implicitly
  ;; closed when `</div>` arrives, and a following sibling must land as a
  ;; sibling of `<div>`, not get nested inside a still-open `<div>`.
  (let [document (html/parse-into-document "<div><span>text</div><p>after</p>")
        tree (dom/tree document)
        [div p] (:children tree)
        span (first (:children div))]
    (is (= 2 (count (:children tree))))
    (is (= :div (:tag div)))
    (is (= :span (:tag span)))
    (is (= ["text"] (:children span)))
    (is (= 1 (count (:children div))))
    (is (= :p (:tag p)))
    (is (= ["after"] (:children p)))))

(deftest stray-closing-tag-with-no-open-match-leaves-stack-untouched
  ;; `</span>` has no matching open `<span>` on the stack, so it must not pop
  ;; anything (in particular, must not silently close the still-open `<div>`).
  (let [document (html/parse-into-document "<div>a</span><p>after</p></div>")
        tree (dom/tree document)
        div (first (:children tree))
        [text p] (:children div)]
    (is (= 1 (count (:children tree))))
    (is (= :div (:tag div)))
    (is (= "a" text))
    (is (= :p (:tag p)))
    (is (= ["after"] (:children p)))))

(deftest title-content-is-raw-text-not-markup
  ;; A `<title>` body containing `<`/`>` must not be interpreted as markup:
  ;; only the real closing `</title>` terminates it, and `<p>` lands as a
  ;; genuine following sibling, not nested inside a still-open `<title>`.
  (let [document (html/parse-into-document "<title>A < B</title><p>after</p>")
        tree (dom/tree document)
        [title p] (:children tree)]
    (is (= 2 (count (:children tree))))
    (is (= :title (:tag title)))
    (is (= ["A < B"] (:children title)))
    (is (= :p (:tag p)))
    (is (= ["after"] (:children p)))))

(deftest textarea-content-is-raw-text-not-markup
  ;; Same raw-text handling applies to `<textarea>`: code-like default text
  ;; containing `<`/`>` must not corrupt the rest of the document.
  (let [document (html/parse-into-document
                  "<textarea>if (1 < 2) { x(); }</textarea><p>after</p>")
        tree (dom/tree document)
        [textarea p] (:children tree)]
    (is (= 2 (count (:children tree))))
    (is (= :textarea (:tag textarea)))
    (is (= ["if (1 < 2) { x(); }"] (:children textarea)))
    (is (= :p (:tag p)))
    (is (= ["after"] (:children p)))))

(deftest textarea-content-that-looks-like-a-tag-is-one-literal-text-child
  ;; A textarea's content is never real markup, no matter what it looks
  ;; like: a substring that resembles a `<b>...</b>` element must become a
  ;; single literal text child, not a parsed `<b>` element.
  (let [document (html/parse-into-document
                  "<textarea><b>not a real tag</b></textarea>")
        tree (dom/tree document)
        textarea (first (:children tree))]
    (is (= :textarea (:tag textarea)))
    (is (= 1 (count (:children textarea))))
    (is (= "<b>not a real tag</b>" (first (:children textarea))))))

(deftest textarea-default-value-reflects-full-raw-text-content
  ;; The form-default-value projection (`initialize-form-node`'s `:textarea`
  ;; case) must see the full, unmangled raw text as the textarea's content
  ;; -- not a fragment truncated by a `<`/`>` inside it.
  (let [document (html/parse-into-document
                  "<textarea>if (1 < 2) { x(); }</textarea>")
        root (dom/node document (:root document))
        textarea (dom/node document (first (:children root)))]
    (is (= "if (1 < 2) { x(); }" (get-in textarea [:attrs :value])))
    (is (= "if (1 < 2) { x(); }" (get-in textarea [:attrs :default-value])))))
