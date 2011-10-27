;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Statements are a representation of literals, i.e. atomic formulas and
            negations of atomic formulas of propositional and predicate logic."}
    carneades.engine.statement
  (:use clojure.contrib.def
        carneades.engine.utils)
  (:require [clojure.string :as str])
  (:import (java.net URI)))

; language = :en, :de, :fr, etc.

(defrecord Statement
  [id               ; symbol, a propositional "letter"
   wff              ; atomic formula or nil
   positive         ; boolean
   weight           ; nil or 0.0-1.0, default nil
   standard         ; proof-standard
   text])           ; (language -> string) map, natural language formulations of the statement

(defn make-statement
   "key value ... -> statement"
   [& key-values]  
   (merge (Statement. 
            (gensym "s")   ; id
            nil             ; wff
            true            ; positive
            nil             ; weight
            :pe             ; proof standard
            {})             ; text
          (apply hash-map key-values)))

(defn statement? [x] (instance? Statement x))

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
    (boolean? x)))

(defn compound-term? [x]
  (or (list? x) 
      (vector? x)
      (map? x)))

(defn term?
  "datum -> boolean"
  [x]
  (or (variable? x)
      (constant? x)
      (termseq? x)
      (compound-term? x)
      (statement? x)))

(defn term-functor
  "term -> symbol | nil "
  [term]
  (cond 
    (and (list? term) 
         (not (empty? term))
         (symbol? (first term))) (first term)
    (statement? term) (term-functor (:wff term))
    :else nil))

(defn term-args
  "term -> (seq-of term)"
  [term]
  (cond 
    (and (list? term) 
         (not (empty? term))
         (symbol? (first term))) (rest term)
    (list? term) (seq term)
    (vector? term) (seq term)
    (statement? term) (term-args (:wff term))
    (map? term) (seq term)
    :else ()))
  
(declare term=)

(defn statement= 
  [s1 s2]
  (and (statement? s1) 
       (statement? s2)
       (or (= (:id s1) (:id s2))
           (and (not (nil? (:wff s1)))
                (not (nil? (:wff s2)))
                (term= (:wff s1) (:wff s2))))))
  
(defn term=
  [t1 t2]
  (cond 
    (and (variable? t1) (variable? t2)) (= t1 t2)
    (and (constant? t1) (constant? t2)) (= t1 t2)
    (and (compound-term? t1) 
         (compound-term? t2)) (and (= (term-functor t1) 
                                      (term-functor t2)) 
                                   (= (count (term-args t1)) 
                                      (count (term-args t2)))
                                   (every? (fn [p] (term= (first p) (second p)))
                                           (partition 2 (interleave (term-args t1) 
                                                                    (term-args t2)))))
    (and (statement? t1) (statement? t2)) (statement= t1 t2)
    :else false))

(defn ground? [t]
  (cond (variable? t) false
        (constant? t) true
        (compound-term? t) (and (ground? (term-functor t))
                                (ground? (term-args t)))
        (statement? t) (ground? (:wff t))
        :else true))

;; could be rewritten (filter variable? (tree-seq seq? identity s)) 
(defn variables
  "term -> (seq-of symbol)
   Returns a sequence of the variables in the term"
  [t]
  (letfn [(vars [t]
                (cond (variable? t) (list t)
                      (constant? t) nil
                      (compound-term? t) (concat (vars (term-functor t))
                                                 (vars (term-args t)))
                      (statement? t) (vars (:wff t))))]
    (distinct (vars t))))

(defn statement-pos? [s] (:positive s))

(defn statement-neg? [s] (not (:positive s)))

(defn statement-complement [s]
  (assoc s :positive (if (statement-pos? s) false true)))

(defn ¬ [stmt] (statement-complement stmt))

(defn statement-atom
  "statement -> statement
   Returns a positive version of the statement."
  [s]
  (assoc s :positive true))

(defn statement-predicate 
  "statement -> symbol or nil"
  [s]
  (term-functor (:wff s)))

(defn statement-wff 
  "Represents the statement as an s-expression.  Negative statements
   are represented with the form (not P), where P is the wff of the
   statement."
  [s]  
  (if (statement-pos? s)
    (:wff s)
    (list 'not (:wff s))))

(defn sexp->statement
  "s-expression -> statement or nil"
  [sexp]
  (if (compound-term? sexp)
    (if (= 'not (first sexp))
      (make-statement :positive false :wff (second sexp))
      (make-statement :positive true :wff sexp))))

(declare term-formatted)

(defn short-str [s]
  (try (or
        (.getFragment (new URI s))
        s)
    (catch Exception e s)))

(defn break-str [s]
  (let [l (.split s " ")]
    (str/join "\n " l)))

(defn statement-formatted
  ([s] (statement-formatted s false :en))
  ([s x] (if (keyword? x) 
             (statement-formatted s true x)
             (statement-formatted s true :en)))
  ([s parentheses? lang]
    (cond
      (string? s) (short-str s),
      (symbol? s) (short-str (str s)),
      (statement? s) (cond (not (empty? (:text s))) (lang (:text s))
                           (:wff s) (if parentheses? 
                                      (str "(" (statement-formatted (statement-wff s)) ")")
                                      (statement-formatted (statement-wff s)))
                           :else (str (:id s)))
      (nonemptyseq? s) (if parentheses?
                         (str "(" (str/join " " (map term-formatted s)) ")")
                         (str/join " " (map term-formatted s))),
      :else s)))

(defn term-formatted [t]
  (cond 
    (or (variable? t) (constant? t)) (short-str (str t)),
    (nonemptyseq? t) (str "(" (str/join " " (map term-formatted t)) ")"),
    (statement? t) (str \" (statement-formatted t true) \")
    :else t))

(defn replace-var
  [from to stmt]
  (cond
    (seq? stmt) (map (fn [t] (replace-var from to t)) stmt),
    (statement? stmt) (assoc stmt :wff (replace-var from to (:wff stmt))),
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
      (make-statement :wff s)
      (sexp->statement (read-string s)))
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
