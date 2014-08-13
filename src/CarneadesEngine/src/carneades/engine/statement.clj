;; Copyright (c) 2010-2011 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.


(ns ^{:doc "Statements are annotated ground literals, i.e. ground atomic formulas and
            negations of atomic formulas of propositional and predicate logic."}
    carneades.engine.statement
  (:use carneades.engine.utils
        carneades.engine.uuid)
  (:require [clojure.string :as str])
  (:import (java.net URI)))

(defn sliteral?
  "A sliteral is an sexpression representation of a literal.
   Variables are also considered to be literals,
   to support some meta-level reasoning."
  [sexp]
  (or (symbol? sexp)
      (and (seq? sexp)
           (not (empty? sexp))
           (symbol? (first sexp)))))

(defn sliteral-pos? [literal]
  {:pre [(sliteral? literal)]}
  (or (not (seq? literal))
      (and (not (empty? literal))
           (not= (first literal) 'not))))

(defn sliteral-atom
  [literal]
  {:pre [(sliteral? literal)]}
  (if (sliteral-pos? literal)
    literal
    (second literal)))

; language = :en, :de, :fr, etc.

(defrecord Statement  ;; statements are annotated literals
  [id                 ; URN symbol
   atom               ; sliteral or nil
   header             ; nil or dublin core metadata description of the model
   positive           ; boolean
   weight             ; nil or 0.0-1.0, default nil
   value              ; nil or 0.0-1.0, default nil
   main               ; true if the statement is a main issue
   standard           ; proof-standard
   text])             ; (language -> string) map, natural language formulations of the statement


(defn map->statement
  [m]
  {:pre [(map? m)]}
  (let [m2 (merge (Statement.
                    nil             ; id
                    nil             ; atom
                    nil             ; header
                    true            ; positive
                    nil             ; weight
                    nil             ; value
                    false           ; main issue
                    :pe             ; proof standard
                    {})             ; text
                  m)
        ; set the id
        m3 (assoc m2
                  :id (if (:id m)
                        (:id m)
                        (make-urn-symbol)))]
    ; normalize the statment
    (if (not (:atom m3))
      m3
      (assoc m3
        :atom (when (:atom m3) (sliteral-atom (:atom m3)))
        :positive (or (and (sliteral-pos? (:atom m3))
                           (:positive m3))
                      (and (not (sliteral-pos? (:atom m3)))
                           (not (:positive m3))))))))

(defn make-statement
   "key value ... -> statement"
   [& key-values]
   (map->statement (apply hash-map key-values)))


(defn statement? [x] (instance? Statement x))


(defn literal?
  [x]
  (or (statement? x)
      (sliteral? x)))

; <constant> := <symbol> | <<number> | <boolean> | <string>
; <term> := <variable> | <constant> | <compound-term> | <statement>

; Note: statements are terms to allow meta-level propositions about statements

(defn variable?
  "object -> boolean
  A logical variable is represented as a symbol prefixed with a
  question mark.  For example: '?x "
  [x]
  (and (symbol? x)
    (let [s (str x)]
      (and (pos? (.length s))
        (= (.charAt s 0) \?)))))

(defn constant?
  "object -> boolean
   Returns true if the object is a constant term"
  [x]
  (or (and (symbol? x) (not (variable? x)))
    (number? x)
    (string? x)
    (boolean? x)
    (keyword? x)))

(defn compound-term? [term]
  (or (and (seq? term)
         (not (empty? term))
         (and (symbol? (first term))
              ;; (not (variable? (first term)))
              ))
      (and (statement? term)
           (seq? (:atom term)))))

(defn termseq? [term]
  (or (vector? term)
      (and (seq? term) (not (compound-term? term)))
      (and (map? term) (not (statement? term)))))

(defn term?
  "datum -> boolean"
  [x]
  (or (variable? x)
      (constant? x)
      (statement? x)
      (compound-term? x)
      (termseq? x)))

(defn term-functor
  "term -> symbol | nil "
  [term]
  (cond
   (and (seq? term)
        (compound-term? term)
        (not (empty? term))) (first term)
        (and (statement? term)
             (:positive term)
             (seq? (:atom term)))
        (recur (:atom term))
        (and (statement? term)
                  (not (:positive term))
                  (seq? (:atom term)))
        'not
        :else nil))

(defn term-args
  "term -> (seq-of term)"
  [term]
  (cond
    (and (seq? term)
         (compound-term? term)) (rest term)
    (seq? term) (seq term)
    (vector? term) (seq term)
    (and (statement? term)
         (:positive term)
         (seq? (:atom term))) (recur (:atom term))
    (and (statement? term)
         (seq? (:atom term))
         (not (:positive term))) (:atom term)
    (map? term) (seq term)
    :else ()))

(defn term-arity
  "compound-term -> integer"
  [term]
  {:pre [(compound-term? term)]}
  (dec (count term)))

(declare term=)

(defn atom?
  "An atom is an sexpression that is
   either a symbol, representing a propositional letter,
   or a non-empty list whose first element is a symbol,
   representing an atomic formula of first-order predicate logic."
  [sexp]
  (or (symbol? sexp)
      (and (seq? sexp)
           (not (empty? sexp))
           (symbol? (first sexp)))))

(defn literal-pos? [literal]
  {:pre [(literal? literal)]}
  (cond (sliteral? literal)
        (or (not (seq? literal))
            (and (not (empty? literal))
                 (not= (first literal) 'not))),
        (statement? literal) (:positive literal)))

(defn literal-neg? [literal]
  {:pre [(literal? literal)]}
  (not (literal-pos? literal)))

(declare literal-complement)

(defn literal-atom
  [literal]
  (cond (sliteral? literal) (if (literal-pos? literal)
                              literal
                              (second literal)),
        (or (statement? literal) (map? literal))
        (or (:atom literal) (:id literal))))

;; it's really unexpected to get a literal when given a sliteral
;; maybe we should change the API?
(defn positive-statement [literal]
  "Returns the atom of the literal in the form
   of a positive statement. If the literal is an sliteral,
   a statement is constructed with the atom of the literal as its atom.
   If the literal is a statement, its :positive attribute is set to true
   and all of its other properties are preserved in the returned statement."
  {:pre [(literal? literal)]}
  (cond (sliteral? literal) (make-statement :atom (literal-atom literal))
        (statement? literal) (assoc literal :positive true)))

(defn propositional?
  [x]
  {:pre [(literal? x)]}
 (symbol? (literal-atom x)))

(defn literal-complement
  [literal]
  {:pre [(literal? literal)]}
  (cond (sliteral? literal)
          (if (literal-pos? literal)
            (list 'not literal)
            (literal-atom literal)),
        (statement? literal)
          (assoc literal
               :positive
               (if (literal-pos? literal) false true))))

(defn statement=
  [s1 s2]
  {:pre [(statement? s1) (statement? s2)]}
  (and (= (:positive s1) (:positive s2))  ; must have same polarity
       (term= (literal-atom s1) (literal-atom s2))))

(defn term=
  [t1 t2]
  (cond
    (and (compound-term? t1)
         (compound-term? t2)) (and (= (term-functor t1)
                                      (term-functor t2))
                                   (= (count (term-args t1))
                                      (count (term-args t2)))
                                   (every? (fn [p] (term= (first p) (second p)))
                                           (partition 2 (interleave (term-args t1)
                                                                    (term-args t2)))))
    (and (statement? t1) (statement? t2)) (statement= t1 t2)
    :else (= t1 t2)))

(defn ground? [t]
  (cond (nil? t) true,
        (and (seq? t) (empty? t)) true,
        (variable? t) false,
        (constant? t) true,
        (statement? t) (ground? (literal-atom t)),
        (coll? t) (and (ground? (first t))
                       (ground? (rest t)))))

;; could be rewritten (filter variable? (tree-seq seq? identity s))
(defn variables
  "term -> (seq-of symbol)
   Returns a sequence of the variables in the term"
  [t]
  (letfn [(vars [t]
            (cond (variable? t) (list t)
                      (constant? t) ()
                      (compound-term? t) (concat (vars (term-functor t)) (vars (term-args t)))
                      (and (or (vector? t)
                               (seq? t)
                               (map? t))
                           (not (empty? t))) (concat (vars (first t))
                                                     (vars (rest t)))

                           (literal? t) (vars (literal-atom t))
                           :else ()))]
    (distinct (vars t))))

(defn neg [literal] (literal-complement literal))

(defn literal-predicate
  "literal -> symbol or nil
   Returns the predicate of the literal of the statement, if it is a predicate
   logic statement, or nil, if it is a propositional logic statement."
  [s]
  (term-functor (literal-atom s)))

(defn atom-predicate
  "atom -> symbol or nil
   Returns the predicate symbol of predicate logic atoms or nil
   if the atom is propositional."
  [atom]
  {:pre [(atom? atom)]}
  (term-functor atom))

(defn literal->statement
  "literal -> statement
   Assures that a literal is in the form of a statement."
  [x]
  {:pre [(literal? x)]}
  (if (statement? x) x
    (if (literal-pos? x)
      (make-statement :positive true :atom x)
      (make-statement :positive false :atom (literal-atom x)))))

(defn literal->sliteral
  "literal -> sliteral
   Assures that a literal is in the form of an sliteral."
  [x]
  {:pre [(literal? x)]}
  (if (sliteral? x) x
    (if (literal-pos? x)
      (literal-atom x)
      (list 'not (literal-atom x)))))

(defn- sliteral->str
  [s]
  (str s))

(defn- statement->str
  [s lang]
  (if-let [translated (get-in s [:text lang])]
    translated
    (sliteral->str (literal-atom s))))

(defn literal->str
  "Returns the string representation of a literal."
  ([l]
     {:pre [(literal? l)]}
     (literal->str l :en))
  ([l lang]
     {:pre [(literal? l)]}
     (cond (sliteral? l) (sliteral->str l)
           (statement? l) (statement->str l lang))))

(defn replace-var
  [from to stmt]
  (cond
    (seq? stmt) (map (fn [t] (replace-var from to t)) stmt),
    (statement? stmt) (assoc stmt :atom (replace-var from to (:atom stmt))),
    :else (if (= stmt from) to stmt)))

(defn sentence?
  "Returns true if the string s contains at least two words and does
   not begin with a parenthesis or a double quote"
  [s]
  (let [s (str/trim s)]
    (and (not= (first s) \()
         (not= (first s) \")
         (> (count (str/split s #"\s+")) 1))))

(defn str-stmt
  "Converts the string s into a statement"
  [s]
  (try
    (if (sentence? s)
      (make-statement :text {:en s})
      (literal->statement (read-string s)))
    (catch Exception e nil)))

(defn stmt-str [stmt]
  (pr-str stmt))

(defn str-term
  "Converts the string s into a term"
  [s]
  (try
    (if (sentence? s)
      s
      (let [term (read-string s)]
        (if (term? term)
          term
          nil)))
    (catch Exception e nil)))

(defn term-str [term]
  (pr-str term))
