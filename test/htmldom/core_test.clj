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
