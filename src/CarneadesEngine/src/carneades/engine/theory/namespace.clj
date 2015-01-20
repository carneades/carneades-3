;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Namespaces for theories."}
  carneades.engine.theory.namespace
  (:require [clojure.walk :as w]
            [clojure.zip :as z]
            [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [carneades.engine.theory.zip :as tz]
            [carneades.engine.statement :as st]
            [carneades.engine.argument :as a]
            [taoensso.timbre :as timbre :refer [debug warn info spy]])
  (:refer-clojure :exclude [name namespace]))

(defn uri?
  "Returns true if the sexp represents an uri."
  [sexp]
  ;; TODO: would match only URL for now
  (re-matches #"^.+://.+" (pr-str sexp)))

(defn namespace
  "Returns a string describing the namespace of atom."
  [atom]
  (let [n (str atom)
        match (re-seq #"(.+):.+" n)]
    (if match
      (second (first match))
      "")))

(defn name
  "Returns a string describing the name of atom."
  [atom]
  (let [n (str atom)
        match (re-seq #".+:(.+)" n)]
    (if match
      (second (first match))
      (str atom))))

(defn to-absolute-atom
  "Converts this atom to an absolute atom. Logs a warning if a
  namespace is missing but necessary for the transformation."
  [atom namespaces]
  (cond (st/variable? atom) atom
        (symbol? atom) (if (re-matches #"^.+://.+" (str atom))
                         atom
                         (let [ns (namespace atom)
                               n (name atom)
                               iri (namespaces ns)]
                           (if (and (nil? iri) (not= ns ""))
                             (do
                               (warn (str "Missing namespace '" ns "' ? " {:atom atom :type (type atom)}))
                               atom)
                             (symbol (str iri n)))))
        :else
        (doall (map #(to-absolute-atom % namespaces) atom))))

(defn to-relative-atom
  "Converts this atom to a relative atom."
  [atom namespaces]
  (let [rnamespaces (set/map-invert namespaces)]
    (cond (st/variable? atom) atom
          (symbol? atom) (let [n (str atom)]
                           (if (re-matches #"^.+://[^:]+" n)
                             (let [s (reduce (fn [n [namespace prefix]]
                                               (if (empty? prefix)
                                                 n
                                                 (s/replace n namespace (str prefix ":"))))
                                             n
                                             rnamespaces)
                                   s (if-let [namespace (get namespaces "")]
                                       (s/replace s namespace "")
                                       s)]
                               (symbol s))
                             atom))
          (string? atom) atom
          :else
          (doall (map #(to-relative-atom % namespaces) atom)))))

(defn to-absolute-statement
  "Converts this statement to an absolute statement."
  [statement namespaces]
  (update-in statement [:atom] to-absolute-atom namespaces))

(defn to-relative-statement
  "Converts this statement to a relative statement."
  [statement namespaces]
  (update-in statement [:atom] to-relative-atom namespaces))

(defn to-absolute-literal
  "Recursively convert each of the atom of the literal to an absolute atom."
  [literal namespaces]
  (cond (empty? namespaces) literal
        (st/statement? literal) (to-absolute-statement literal namespaces)
        :else (to-absolute-atom literal namespaces)))

(defn to-relative-literal
  "Recursively convert each of the atom of the literal to a relative atom."
  [literal namespaces]
  (cond (empty? namespaces) literal
        (st/sliteral? literal) (to-relative-atom literal namespaces)
        :else (to-relative-statement literal namespaces)))

(defn to-absolute-premise
  "Converts a premise to an absolute premise"
  [premise namespaces]
  (if (st/literal? premise)
    (to-absolute-literal premise namespaces)
    (update-in premise [:statement] to-absolute-literal namespaces)))

(defn to-absolute-premises
  "Converts a seq of premises to absolute premises"
  [premises namespaces]
  (into [] (map #(to-absolute-premise % namespaces) premises)))

(defn to-absolute-scheme
  "Converts the scheme to an absolute scheme."
  [scheme namespaces]
  (-> scheme
      (update-in [:conclusion] to-absolute-literal namespaces)
      (update-in [:premises] to-absolute-premises namespaces)
      (update-in [:exceptions] to-absolute-premises namespaces)
      (update-in [:assumptions] to-absolute-premises namespaces)))

(defn to-absolute-schemes
  "Converts the schemes to absolute schemes."
  [schemes namespaces]
  (into [] (map #(to-absolute-scheme % namespaces) schemes)))

(defn to-absolute-section
  "Transforms the atoms contained in the schemes of a section to absolute atoms."
  [section namespaces]
  (update-in section [:schemes] to-absolute-schemes namespaces))

(defn to-absolute-theory
  "Converts the atoms contained in the schemes of the theory to
  absolute atoms."
  ([theory]
     (to-absolute-theory theory (:namespaces theory)))
  ([theory namespaces]
     (loop [loc (tz/theory-zip theory)]
       (if (z/end? loc)
         (z/root loc)
         (let [loc (z/edit loc to-absolute-section namespaces)]
           (recur (z/next loc)))))))
