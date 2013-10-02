(ns ^{:doc "Translation from literals to human-readable strings.

  Translators are passed a context with a :literal and a :lang keys and can augment
  the context with new information for the other translators or
  provide, or modify, a translation returned by augmenting the context
  with a :translation key.

  Translators can be combined together with clojure.core/comp."}
  carneades.engine.translation
  (:require [carneades.engine.statement :as st]
            [carneades.engine.argument-graph :as ag]))

(defn translate-ag
  "Translates the argument graph with a translator."
  [ag translator]
  (reduce (fn [ag stmt-node]
            (let [stmt (st/map->statement stmt-node)
                  translation (:translation (translator {:literal stmt}))]
              (ag/update-statement-node ag stmt-node :text {:en translation})))
          ag
          (vals (:statement-nodes ag))))

(defn get-lang
  [context]
  (or (:lang context) :en))

(defn translate-literal
  [context]
  (let [literal (:literal context)
        virtual-atom (or (:virtual-atom context) literal)
        lang (get-lang context)]
    (cond (and (st/statement? literal) (get-in literal [:text lang]))
          (if-let [txt (get-in literal [:text lang])]
            txt
            (pr-str (st/literal-atom literal)))

          (st/statement? literal) (pr-str (st/literal-atom literal))

          :else (pr-str virtual-atom))))

(defn make-default-translator
  "Returns a default translator for literals. It uses the :text field
  of statement or the atom represented a string."
  []
  (fn [context]
    (if (:translation context)
      context
      (let [translation (translate-literal context)]
        (assoc context :translation translation)))))

(defn make-prefix-translator
  "Examples of a translator modifying "
  [prefix]
  (fn [context]
    (assoc context :translation (str prefix (:translation context)))))
