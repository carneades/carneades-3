;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Translation from a language."}
  carneades.engine.theory.translation
  (:require [clojure.pprint :refer [pprint]]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.theory :as t]
            [clojure.zip :as z]
            [carneades.engine.theory.zip :as tz]
            [carneades.engine.theory.namespace :as n]
            [carneades.engine.statement :as st]
            [carneades.engine.translation :as tr]))

(declare format-literal-args)

(defn format-literal-arg
  "Format the argument of a literal."
  [arg language lang]
  (cond (and (symbol? arg) (language arg))
        (or (get-in language [arg :text lang])
            (get-in language [arg :text :en]))

        (and (st/literal? arg) (language (st/literal-predicate (st/literal-atom arg))))
        (let [pred (st/literal-atom (st/literal-predicate arg))
              fstring (or (get-in language [pred :text lang])
                          (get-in language [pred :text :en]))]
          (apply format fstring (format-literal-args arg language lang)))

        (st/variable? arg) (subs (str arg) 1)

        :else (str arg)))

(defn format-literal-args
  "Format the arguments of a literal"
  [literal language lang]
  (map #(format-literal-arg % language lang) (rest (st/literal-atom literal))))

(defn has-translation?
  [language pred lang]
  (and (language pred)
       (get-in language [pred :forms lang])))

(defn make-language-translator
  "Returns a translator translating literals with the help of forms
  defined in a language."
  [language]
  (fn [context]
    (let [atom (or (:virtual-atom context) (st/literal-atom (:literal context)))
          pred (st/literal-predicate (:literal context))
          lang (tr/get-lang context)]
      (if (has-translation? language pred lang)
        (let [fstring (get-in language [pred :forms lang (context :direction :positive)])
              translation (apply format fstring (format-literal-args atom language lang))]
          (assoc context :translation translation))
        context))))

(defn make-uri-shortening-translator
  "Returns a translator shortening the URI with namespaces"
  [namespaces]
  (fn [context]
    (let [atom (or (:virtual-atom context) (st/literal-atom (:literal context)))
          short-atom (n/to-relative-atom atom namespaces)]
      (assoc context :virtual-atom short-atom))))

(defn add-premise-translation
  [translator lang premise]
  (assoc premise :translation (:translation (translator {:literal (:statement premise)
                                                         :lang lang}))))

(defn translate-premises
  [premises translator lang]
  (into [] (map (partial add-premise-translation translator lang) premises)))

(defn translate-scheme
  [scheme translator lang]
  (assoc scheme
    :conclusion-translation (:translation (translator {:literal (:conclusion scheme)
                                                       :lang lang}))
    :premises (translate-premises (:premises scheme) translator lang)
    :assumptions (translate-premises (:premises scheme) translator lang)
    :exceptions (translate-premises (:premises scheme) translator lang)))

(defn translate-schemes
  [translator lang section]
  (assoc section
    :schemes
    (into [] (map #(translate-scheme % translator lang) (:schemes section)))))

(defn translate-theory
  "Translates a theory using a translator."
  [theory translator lang]
  (tz/map-theory theory (partial translate-schemes translator lang)))
