;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.statement
  (:use clojure.test clojure.contrib.def
    carneades.engine.utils)
  (:require [clojure.contrib.str-utils2 :as s])
  (:import
    (java.net URI)))

(defn variable? [x]
  "object -> boolean
Returns true if x is a variable
A logic variable is represented as a symbol prefixed with a
question mark.  For example: '?x
"
  (and (symbol? x)
    (let [s (str x)]
      (and (pos? (.length s))
        (= (.charAt s 0) \?)))))

(defn constant? [x]
  "object -> boolean
Returns true if x is a constant"
  (or (and (symbol? x) (not (variable? x)))
    (number? x)
    (string? x)
    (boolean? x)))


; <term> := <atom> |
; <atom> := <symbol> | <list> | <string> | <fatom>
; <statement> := <atom> | (not <atom>)   ;; i.e. literals

;; formatted atomic formulas of predicate logic.
;;   :form is a string, with %s, as in format, used to denote fields
;;   Example: (struct fatom \"The mother of %s is %s.\" '(mother Tom Gloria))
(defstruct fatom
  :form
  :term)

(defn fatom? [s]
  (and (map? s)
    (let [f (get s :form 'notfound)
          t (get s :term 'notfound)]
      (and (not= f 'notfound)
        (not= t 'notfound)))))

(defn compound-term? [x]
  (or (nonemptyseq? x)
    (fatom? x)))

(defn term? [x]
  "datum -> boolean"
  (or (variable? x)
      (constant? x)
      (compound-term? x)))

(defn term-functor [t]
  "term -> symbol | nil "
  (cond (nonemptyseq? t) (first t)
    (fatom? t) (first (:term t))))

(defn term-args [t]
  "term -> (seq-of term)"
  (cond (nonemptyseq? t) (rest t)
    (fatom? t) (rest (:term t))
    :else '()))

(defn statement= [t1 t2]
  (cond (and (variable? t1) (variable? t2))
    (= t1 t2)
    (and (constant? t1) (constant? t2))
    (= t1 t2)
    (and (compound-term? t1) (compound-term? t2))
    (and (= (term-functor t1) (term-functor t2))
      (= (count (term-args t1)) (count (term-args t2)))
      (every? (fn [p] (statement= (first p) (second p)))
        (partition 2 (interleave (term-args t1) (term-args t2)))))
    :else false))

(defn ground? [t]
  (cond (variable? t) false
    (constant? t) true
    (compound-term? t) (and (ground? (term-functor t))
                         (ground? (term-args t)))
    :else true))

;; could be rewritten (filter variable? (tree-seq seq? identity s)) ?
(defn variables [t]
  "term -> (seq-of symbol)
Returns a sequence of the variables in the term"
  (letfn [(vars [t]
            (cond (variable? t) (list t)
              (constant? t) nil
              (compound-term? t) (concat (vars (term-functor t))
                                   (vars (term-args t)))))]
    (distinct (vars t))))

(defn statement? [s]
  (or (symbol? s)
    (string? s)
    (nonemptyseq? s)
    (fatom? s)))

(defn statement-pos? [s]
  (not (and (nonemptyseq? s)
         (= (first s) 'not))))

(defn statement-neg? [s]
  (not (statement-pos? s)))

(defn statement-complement [s]
  (if (nonemptyseq? s)
    (if (statement-pos? s)
      (list 'not s)
      (fnext s))
    (list 'not s)))

(defn statement-atom [s]
  "statement -> statement
Returns the atom of the statement, i.e strips the negation operator if there
is one"
  (if (statement-neg? s)
    (fnext s)
    s))

(defn statement-compare [s1 s2]
  (cond (and (statement-pos? s1) (statement-pos? s2))
    (cond (and (fatom? s1) (fatom? s2))
      (compare (:term s1) (:term s2))
      (and (nonemptyseq? s1) (fatom? s2))
      (compare s1 (:term s2))
      (and (fatom? s1) (nonemptyseq? s2))
      (compare (:term s1) s2)
      ;; no compare for lists!,
      ;; see http://groups.google.com/group/clojure/msg/99fa7e6c6bf79330
      :else (compare (vec s1) (vec s2)))
    (and (statement-neg? s1) (statement-neg? s2))
    (statement-compare (statement-atom s1) (statement-atom s2))
    (and (statement-pos? s1) (statement-neg? s2))
    1
    (and (statement-neg? s1) (statement-pos? s2))
    -1))

(defn statement-predicate [s]
  (let [atm (statement-atom s)]
    (cond (nonemptyseq? atm) (first atm)
      (fatom? atm) (first (:term atm)))))

(defn statement-symbol [s]
  (let [a (statement-atom s)]
  (or (statement-predicate a)
    (if (string? a)
      (symbol a)
      a))))

(defn statement-wff [s]
  (if (statement-pos? s)
    (if (fatom? s)
      (:term s)
      s)
    (list 'not (statement-wff (statement-atom s)))))

(declare term-formatted)

(defn short-str [s]
  (try (or
        (.getFragment (new URI s))
        s)
    (catch Exception e s)))

(defn break-str [s]
  (let [l (.split s " ")]
    (s/join "\n " l)))

(defn statement-formatted
  ([s] (statement-formatted s false))
  ([s parentheses?]
    (cond
      (string? s) (short-str s),
      (symbol? s) (short-str (str s)),
      (fatom? s) (apply format `(~(:form s)
                                  ~@(map term-formatted (rest (:term s))))),
      (nonemptyseq? s) (if parentheses?
                         (str "(" (s/join " " (map term-formatted s)) ")")
                         (s/join " " (map term-formatted s))),
      true s)))

(defn term-formatted [t]
  (cond (or (variable? t) (constant? t) ) (short-str (str t))
    (nonemptyseq? t) (str "(" (s/join " " (map term-formatted t)) ")")
    (fatom? t) (str \" (statement-formatted t true) \")
    true t))

(defn replace-var
  [from to stmt]
  (cond
    (seq? stmt) (map (fn [t] (replace-var from to t)) stmt),
    (fatom? stmt) (assoc stmt :term (replace-var from to (:term stmt))),
    true (if (= stmt from)
           to
           stmt)))

;; OLD one:
;; (deftrace str-to-stmt [s]
;;   (letfn [(sexp? [s] (= \( (first s)))
;;           (check-stmt [s] (when (and (statement? s) (not (empty? s))) s))]
;;     (when (not (nil? s))
;;       (let [s (str/trim s)]
;;         (if (sexp? s)
;;           (try
;;             (let [sexp (read-string s)]
;;               (check-stmt sexp))
;;             (catch Exception e
;;               nil))
;;           (check-stmt s))))))

(defn str-stmt [s]
  "converts the string s into a statement"
  (try
    (let [stmt (read-string s)]
      (if (statement? stmt)
        stmt
        nil))
    (catch Exception e nil)))

(defn stmt-str [stmt]
  (pr-str stmt))

(defn str-term [s]
  "converts the string s into a term"
  (try
    (let [term (read-string s)]
      (if (term? term)
        term
        nil))
    (catch Exception e nil)))

(defn term-str [term]
  (pr-str term))
