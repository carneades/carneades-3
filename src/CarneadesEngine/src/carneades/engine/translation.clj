;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Translation from literals to human-readable strings.

  Translators are passed a context with a :literal and a :lang keys and can augment
  the context with new information for the other translators or
  provide, or modify, a translation returned by augmenting the context
  with a :translation key.

  Translators can be combined together with clojure.core/comp."}
  carneades.engine.translation
  (:require [carneades.engine.statement :as st]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.utils :refer [serialize-atom unserialize-atom]]
            [taoensso.timbre :as timbre :refer [debug info]]))

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
        virtual-atom (or (:virtual-atom context) (:atom context))
        lang (get-lang context)]
    (if (st/statement? literal)
      (if-let [txt (get-in literal [:text lang])]
        txt
        (pr-str virtual-atom))
          
      (pr-str virtual-atom))))

(defn make-default-translator
  "Returns a default translator for literals. It uses the :text field
  of statement or the atom represented as a string."
  []
  (fn [context]
    (if (:translation context)
      context
      (let [translation (translate-literal context)]
        (assoc context :translation translation)))))

(defn- remove-literal-variables
   [l]
   (if (seq? l)
     (apply list (map remove-literal-variables l))
     (if (st/variable? l)
       (unserialize-atom (subs (serialize-atom l) 1))
       l)))

(defn variable-converter-translator
  "Changes variables symbols to normal symbols (non-recursively)."
  []
  (fn [context]
    (let [atom (or (:virtual-atom context) (st/literal-atom (:literal context)))]
      (assoc context :virtual-atom (remove-literal-variables atom)))))

(defn make-prefix-translator
  "Examples of a translator modifying "
  [prefix]
  (fn [context]
    (assoc context :translation (str prefix (:translation context)))))
