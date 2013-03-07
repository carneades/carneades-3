;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Some utilities to format a statement"}
  carneades.policy-analysis.web.views.pmt.theory
  (:use [jayq.util :only [log clj->js]])
  (:require [clojure.string :as s]))

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

(defn statement? [x] false)

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
      (and (pos? (count s))
           (= (aget s 0) \?)))))

(defn constant?
  "object -> boolean
   Returns true if the object is a constant term"
  [x]
  (or (and (symbol? x) (not (variable? x)))
    (number? x)
    (string? x)
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
  {:pre [(literal? literal)]}
  (cond (sliteral? literal) (if (literal-pos? literal) 
                              literal
                              (second literal)),
        (statement? literal) (or (:atom literal) 
                                 (:id literal))))

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
                ; (println t)
                (cond (variable? t) (list t)
                      (constant? t) ()
                      (compound-term? t) (recur (term-args t))
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

(defn stmt-str [stmt]
  (pr-str stmt))

(defn term-str [term]
  (pr-str term))

(declare format-literal-args)

(defn format-literal-arg
  "Format the argument of a literal. The argument can be a carneades.engine.scheme.Function
call or a symbol."
  [arg language lang]
  ;; {:post [(do (log "arg =") (log arg) (log "result =>") (log %) true)]}
  (cond (and (symbol? arg) (language (keyword arg)))
        (or (get-in language [(keyword arg) :text lang])
            (get-in language [(keyword arg) :text :en]))
        (and (literal? arg) (language (keyword (literal-predicate (literal-atom arg)))))
        (let [pred (keyword (literal-atom (literal-predicate arg)))
              fstring (or (get-in language [pred :text lang])
                          (get-in language [pred :text :en]))]
          (apply format fstring (format-literal-args arg language lang)))
        :else (str arg)))

(defn format-literal-args
  "Format the arguments of a literal"
  [literal language lang]
  ;; (log "rest =")
  ;; (log (rest (literal-atom literal)))
  ;; (log "formatted as")
  ;; (log (map #(format-literal-arg % language lang) (rest (literal-atom literal))))
  (map #(format-literal-arg % language lang) (rest (literal-atom literal))))

(defn array->literal
  [x]
  (let [c (js->clj x)]
    (cond (vector? c)
          (apply list (map array->literal c))
          (string? c)
          (symbol c)
          :else c)))

(defn ^:export format-statement
  "Uses the formular to returns a user-readable sentence describing the literal.
   Selector is positive, negative or question
Language is a ClojureScript map.
 (for efficiency reason the conversion is not done in this function)."
  [literal language lang selector]
  (let [literal (array->literal literal)
        pred (keyword (literal-predicate literal))
        lang (keyword lang)
        selector (keyword selector)]
    (cond (string? literal)
          literal
          (get-in language [pred])
          ;; statement's predicate is in the language
          (let [fstring (get-in language [pred :forms lang selector])]
            (apply format fstring (format-literal-args literal language lang)))
          :else (str literal))))

(defn ^:export convert-language
  "Convert the JS language to the ClojureScript equivalent"
  [language]
  (js->clj language :keywordize-keys true))
