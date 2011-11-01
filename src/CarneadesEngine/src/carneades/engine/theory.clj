;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Theories defined using argumentation schemes."}
  carneades.engine.theory
            
  (:use 
    carneades.engine.statement
    carneades.engine.unify
    carneades.engine.argument
    carneades.engine.argument-generator
    carneades.engine.dublin-core
    [carneades.engine.utils :only (mapinterleave)]))

(defrecord Form
  [positive     ; string for the positive sentences
   negative     ; string for the negative sentences
   question])   ; string for questions

(defn make-form 
  "key value ... -> form"
  [& key-values]  
  (merge (Form. 
           ""    ; positive
           ""    ; negative
           "")   ; question
         (apply hash-map key-values)))

(defn form? [x] (instance? Form x))
  
(defrecord Predicate 
  [symbol   ; symbol
   arity    ; integer
   forms])  ; (lang -> form) map, where lang is one of the keywords :en, :de, etc.

(defn make-predicate
  "key value ... -> predicate"
  [& key-values]  
  (merge (Predicate. 
           (gensym "p")    ; symbol
           0               ; arity
           {})             ; forms
         (apply hash-map key-values)))

(defn predicate? [x] (instance? Predicate x))

(defrecord Individual
  [symbol   ; symbol
   text])   ; (lang -> string) map.

(defn make-individual
  "key value ... -> predicate"
  [& key-values]  
  (merge (Individual. 
           (gensym "p")    ; symbol
           {})             ; text map
         (apply hash-map key-values)))

(defn individual? [x] (instance? Individual x))

; variables are allowed as conclusions of clauses to enable them
; to represent argumentation schemes, such as arguments from 
; expert witness testimony, whose conclusions can be any (literal) proposition.

(defrecord Clause
  [id            ; symbol
   conclusions   ; sequence of literals or variables (ranging over literals)
   strict        ; boolean, defeasible if false
   weight        ; nil or number in the range 0.0 to 1.0
   premises      ; (string -> wff) map, where the string is the role of each premise
   exceptions    ; sequence of wffs
   assumptions   ; sequence of wffs
   scheme])      ; the scheme to which this clause belongs; creates a cyclic structure when used.

; TO DO: allow premises to be a vector in calls to make-clause, as in make-argument

; The scope of clause ids is local to their scheme. 

(defn make-clause
  "key value ... -> clause"
  [& key-values]  
  (merge (Clause. 
           (gensym "c")    ; id 
           []              ; conclusions
           false           ; strict
           nil             ; weight
           {}              ; premises
           []              ; exceptions
           []              ; assumptions
           nil)            ; the scheme of this clause
         (apply hash-map key-values)))

(defn axiom 
  "literal -> clause"
  [literal]
  (make-clause :strict true :conclusions [literal]))

(defn clause? [x] (instance? Clause x))

; The scope of scheme ids is their theory

(defrecord Scheme
  [id          ; symbol
   name        ; string
   clauses     ; sequence of clauses
   source])    ; source or nil

(defn make-scheme
  "key value ... -> scheme"
  [& key-values]  
  (merge (Scheme. 
           (gensym "?")    ; id
           ""              ; name
           []              ; clauses
           nil)            ; source
         (apply hash-map key-values)))

(defn scheme? [x] (instance? Scheme x))

(defn rename-clause-variables [c]
  (let [[m1 conclusions] (rename-variables {} (:conclusions c)),
        [m2 premise-vals] (rename-variables m1 (vals (:premises c))),
        [m3 exceptions] (rename-variables m2 (:exceptions c))
        [m4 assumptions] (rename-variables m3 (:assumptions c))]
    (assoc c 
           :conclusions conclusions
           :premises (zipmap (keys (:premises c)) premise-vals)
           :exceptions exceptions
           :assumptions assumptions)))

(defrecord Theory
  [name      ; string
   source    ; source
   language  ; (symbol -> individual or predicate) map
   schemes   ; (symbol -> scheme) map
   index])   ; (symbol -> seq of clauses) map; see index-key 
 
(defn make-theory
  "key value ... -> theory"
  [& key-values]  
  (merge (Theory. 
           ""              ; name
           nil             ; source
           {}              ; language
           {}              ; schemes
           {})             ; index
         (apply hash-map key-values)))

(defn theory? [x] (instance? Theory x))
   
(defn- clause-index-key 
  "term -> symbol | nil
   Returns the symbol used to index a clause by its conclusions 
   for quicker retrieval. By default clauses are indexed under nil."
  [trm]
  {:pre [(term? trm)]}
  (cond (constant? trm) trm,
        (variable? trm) nil,
        (compound-term? trm) (term-functor trm)
        (statement? trm) (statement-predicate trm)
        :else nil))
 
(defn- create-indices
  "theory -> theory
   Create an index of the clauses in the theory, to enable more 
   efficient retrieval when constructing arguments."
  [theory1]
  {:pre [(theory? theory1)]}
  (reduce (fn [theory2 scheme]
            (reduce (fn [theory3 clause]
                      (reduce (fn [theory4 conclusion]
                                (assoc theory4 
                                       :index
                                       (assoc (:index theory4)
                                              (clause-index-key conclusion)
                                              (conj (get (:index theory4)
                                                         (clause-index-key conclusion))
                                                    (assoc clause :scheme scheme)))))
                              theory3
                              (:conclusions clause)))
                    theory2
                    (:clauses scheme))
            theory1
            (:schemes theory1))))

(defn- get-clauses 
  "theory goal substititions -> sequence of clauses
   where the goal is a literal."
  [theory goal subs]
  {:pre [(theory? theory) 
         (literal? goal)
         (map? subs)]}
  (get (:index theory)
       (clause-index-key (apply-substitutions subs goal))))

(defn- clause-theory-id
  "Returns an id for a clause which has theory scope. 
   The id is constructed by joining the id of the scheme of
   the clause with the local id of the clause in the scheme.
   The id is used to reify schemes in the theory for
   use in, e.g., (applies ...) and (exlcuded ...) atoms."
  [clause]
  (symbol (str (:id (:scheme clause)) "." (:id clause)))) 

; Rebuttals are generated from the exceptions of clauses, 
; where the rebuttals have the form:
; (make-argument :conclusion (excluded <clause-id> <goal>) :premises [<exception>])

(defn generate-arguments-from-theory
  "theory -> argument-generator"
  [theory1]
  (let [theory2 (create-indices theory1)]
    (reify ArgumentGenerator
      (generate [this goal subs]
                (letfn [(apply-for-conclusion
                          [clause c]
                          ;; apply the clause for conclusion c
                          (let [subs2 (or (unify c goal subs)
                                          (unify `(~'applies ~(clause-theory-id clause) ~c) goal subs))]
                            (if (not subs2)
                              false ; fail
                              (cons (make-response subs2
                                                   (map literal->statement 
                                                        (:assumptions (:clause clause)))  
                                                   (make-argument 
                                                     :conclusion (literal->statement goal)
                                                     :strict (:strict clause)
                                                     :weight (:weight clause)
                                                     :premises (zipmap (keys (:premises clause))
                                                                       (map literal->statement 
                                                                            (vals (:premises clause))))
                                                     :scheme (:name (:scheme clause))))
                                    (map (fn [e] (make-response subs2
                                                                ()
                                                                (make-argument 
                                                                  :conclusion (literal->statement 
                                                                                `(~'excluded ~(clause-theory-id clause)
                                                                                             ~c))
                                                                  :strict false
                                                                  :weight (:weight clause)
                                                                  :premises [(literal->statement e)]
                                                                  :scheme (:name (:scheme clause))))
                                           (:exceptions clause)))))))
                        
                        (apply-clause [clause]
                                      (apply concat (filter identity 
                                                            (map #(apply-for-conclusion clause %) 
                                                                 (:conclusions clause)))))]
                  (mapinterleave
                    (fn [c] (apply-clause c))
                    (map rename-clause-variables (get-clauses theory2 goal subs))))))))

