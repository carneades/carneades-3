;;; Copyright ? 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Statements are annotated literals, i.e. atomic formulas and
            negations of atomic formulas of propositional and predicate logic."}
    carneades.engine.statement
  (:use carneades.engine.utils)
  (:require [clojure.string :as str])
  (:import (java.net URI)))

; language = :en, :de, :fr, etc.

(defrecord Statement  ; statements are annotated literals
  [atom               ; atomic formula of propositional (symbol) or predicate logic (list)
   positive           ; boolean
   weight             ; nil or 0.0-1.0, default nil
   standard           ; proof-standard
   text])             ; (language -> string) map, natural language formulations of the statement

(defn make-statement
   "key value ... -> statement"
   [& key-values]  
   (merge (Statement. 
            (gensym "s")    ; atom
            true            ; positive
            nil             ; weight
            :pe             ; proof standard
            {})             ; text
          (apply hash-map key-values)))

(defn statement? [x] (instance? Statement x))

(defn propositional? 
  [x] 
  (and (instance? Statement x)
       (symbol? (:atom x))))

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
              (not (variable? (first term)))))
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
         (seq? (:atom term))) (recur (:atom term))
    (and (statement? term)
         (not (:positive term))
         (seq? (:atom term))) 'not
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

(defn literal? 
  "A literal is an sexpression representing an atomic formula or
   negated atomic formula. Variables are also considered to 
   be literals, to support some meta-level reasoning." 
  [sexp] 
  (or (symbol? sexp)
      (and (seq? sexp)
           (not (empty? sexp))
           (symbol? (first sexp)))))

(defn literal-pos? [wff] 
  {:pre [(literal? wff)]}
  (or (not (seq? wff))
      (and (not (empty? wff))
           (not= (first wff) 'not))))

(def literal-neg? [wff]
  {:pre [(literal? wff)]}
  (not (literal-pos? wff)))

(defn literal-atom
  [wff]
  {:pre [(literal? wff)]}
  (if (literal-pos? wff) 
    wff
    (second wff)))

(defn literal-complement
  [wff]
  {:pre [(literal? wff)]}
  (if (literal-pos? wff)
    (list 'not wff)
    (literal-atom wff)))

(defn statement= 
  [s1 s2]
  {:pre [(statement? s1) (statement? s2)]}
  (and (= (:positive s1) (:positive s2))  ; must have same polarity
       (term= (:atom s1) (:atom s2))))
  
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
  (cond (variable? t) false
        (constant? t) true
        (compound-term? t) (and (ground? (term-functor t))
                                (ground? (term-args t)))
        (statement? t) (ground? (:atom t))
        :else true))

;; could be rewritten (filter variable? (tree-seq seq? identity s)) 
(defn variables
  "term -> (seq-of symbol)
   Returns a sequence of the variables in the term"
  [t]
  (letfn [(vars [t]
                ; (println t)
                (cond (variable? t) (list t)
                      (constant? t) ()
                      (compound-term? t) (recur (term-args t))
                      (and (or (vector? t)
                               (seq? t)
                               (map? t))
                           (not (empty? t))) (concat (vars (first t))
                                                     (vars (rest t)))
                 
                           (statement? t) (vars (:atom t))
                      :else ()))]
    (distinct (vars t))))

(defn statement-pos? [s] (:positive s))


(defn statement-neg? [s] (not (:positive s)))

(defn statement-complement [s]
  (assoc s :positive (if (statement-pos? s) false true)))

(defn neg [stmt] (statement-complement stmt))

(defn statement-atom
  "statement -> statement
   Returns a positive version of the statement."
  [s]
  (assoc s :positive true))

(defn statement-predicate 
  "statement -> symbol or nil
   Returns the predicate of the wff of the statement, if it is a predicate
   logic statement, or nil, if it is a propositional logic statement."
  [s]
  (term-functor (:atom s)))

(defn atom-predicate
  "atom -> symbol or nil
   Returns the predicate symbol of predicate logic atoms or nil
   if the atom is propositional."
  [atom]
  {:pre [(atom? atom)]}
  (term-functor atom))

(defn statement-wff 
  "Represents the statement as an s-expression.  Negative statements
   are represented with the form (not P), where P is the wff of the
   statement."
  [s]  
  (if (statement-pos? s)
    (:atom s)
    (list 'not (:atom s))))

(defn literal->statement
  "literal -> statement"
  [sexp]
  {:pre [(literal? sexp)]}
  (if (literal-pos? sexp)
      (make-statement :positive true :atom sexp)
      (make-statement :positive false :atom (literal-atom sexp))))

(defn statement->literal
  "statement -> literal"
  [stmt]
  {:pre [(statement? stmt)]}
  (if (statement-pos? stmt)
    (:atom stmt)
    (list 'not (:atom stmt))))

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
             (statement-formatted s x :en)))
  ([s parentheses? lang]
    (cond
      (string? s) (short-str s),
      (symbol? s) (short-str (str s)),
      (statement? s) (cond (not (empty? (:text s))) (lang (:text s))
                           (:atom s) (if (and (seq? (:atom s)) parentheses?) 
                                      (str "(" (statement-formatted (statement-wff s)) ")")
                                      (statement-formatted (statement-wff s))))
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
      (make-statement :atom s)
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
