;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Argumentation schemes and theories defined
            using argumentation schemes."}
  carneades.engine.theory
            
  (:use 
    carneades.engine.statement
    carneades.engine.unify
    carneades.engine.argument
    carneades.engine.argument-generator
    carneades.engine.dublin-core))

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
   forms])  ; (lang -> form) map

(defn make-individual
  "key value ... -> predicate"
  [& key-values]  
  (merge (Individual. 
           (gensym "p")    ; symbol
           {})             ; forms
         (apply hash-map key-values)))

(defn individual? [x] (instance? Individual x))

(defrecord Clause
  [id            ; symbol
   conclusions   ; sequence of wffs
   strict        ; boolean, defeasible if false
   premises      ; (string -> wff) map, where the string is the role of each premise
   exceptions    ; sequence of wffs
   assumptions]) ; sequence of wffs

; TO DO: allow premises to be a vector, as in make-argument

(defn make-clause
  "key value ... -> clause"
  [& key-values]  
  (merge (Clause. 
           (gensym "c")    ; symbol
           []              ; conclusions
           false           ; strict
           {}              ; premises
           []              ; exceptions
           [])             ; assumptions
         (apply hash-map key-values)))

(defn axiom 
  "literal -> clause"
  [literal]
  (make-clause :strict true :conclusions [literal]))

(defn clause? [x] (instance? Clause x))

(defrecord Scheme
  [id          ; symbol (or rather keyword?)
   name        ; string  
   clauses     ; sequence of clauses
   source])    ; source or nil

(defn make-scheme
  "key value ... -> scheme"
  [& key-values]  
  (merge (Scheme. 
           (gensym "s")    ; symbol
           ""              ; name
           []              ; clauses
           nil)            ; source
         (apply hash-map key-values)))

(defn scheme? [x] (instance? Scheme x))
   
(defn- predicate 
  "literal -> symbol
   Returns the symbol used to index clauses by their conclusions 
   for quicker retrieval. For this purpose, the 'predicate' of
   a propositional atom is the atom itself."
  [sexp]
  {:pre [(literal? sexp)]}
  (if (literal-pos? sexp)
    (if (symbol? sexp)
      sexp
      (first sexp))
    (recur (second sexp))))

(defn rename-clause-variables [c]
  (let [[m1 conclusions] (rename-variables {} (:conclusions c)),
        [m2 premises] (rename-variables m1 (:premises c)),
        [m3 exceptions] (rename-variables m2 (:exceptions c))
        [m4 assumptions] (rename-variables m3 (:assumptions c))]
    (assoc c 
           :conclusions conclusions
           :premises premises
           :exceptions exceptions
           :assumptions assumptions)))

(defrecord Theory
  [name      ; string
   source    ; source
   language  ; (symbol -> individual or predicate) map
   schemes   ; (symbol -> scheme) map
   index])   ; (symbol -> clause) map, by predicate symbol

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

; START HERE

(defn add-rule
  "rulebase rule -> rulebase

   Add a rule to the rule base, for each conclusion of the rule,
   indexing it by the predicate of the conclusion.  There will be a copy
   of the rule, each with the same id, for each conclusion of the rule. This
   is an optimization, so that we don't have to iterate over the conclusions
   when trying to unify some goal with the conclusion of the rule
   with some goal."
  [rb r]
  
  (reduce (fn [rb2 conclusion]
            (let [pred (predicate conclusion)
                  table (:table rb2)
                  current-rules (table pred)
                  new-rules (conj current-rules r)]
              (if (not (.contains (map :id current-rules) (:id r)))
                (struct rulebase-struct
                        (assoc table pred new-rules)
                        (conj (:rules rb) r))
                rb2)))
          rb
          (:head r)))

(defn add-rules
  "rulebase (seq-of rule) -> rulebase"
  [rb l]
  (reduce (fn [rb2 r]
            (add-rule rb2 r))
          rb
          l))

(defn rulebase [& l]
  (add-rules *empty-rulebase* l))

(defn- concat-scheme [l]
  (str/join "-" l))

(defn- remove-inst [s]
  (let [sl (.split s "-")]
    (concat-scheme (butlast sl))))

(defn- rule->clauses [rule]                                             
  (let [rule-clauses (:body rule)]
    (if (empty? rule-clauses)
      (list (struct named-clause
                    (gensym "c")
                    (:id rule)
                    (:strict rule)
                    (:domains rule)
                    (:head rule)
                    '()))
      (map (fn [clause] 
             (struct named-clause
                     (gensym "c")
                     (:id rule)
                     (:strict rule)
                     (:domains rule)
                     (:head rule)
                     clause)) 
           rule-clauses))))

(defn get-clauses [rb goal subs]
  (let [pred (predicate (apply-substitutions subs goal))
        applicable-rules ((:table rb) pred)]
    (mapinterleave rule->clauses applicable-rules)))

(defn generate-arguments-from-rules
  ([rb] (generate-arguments-from-rules rb nil))
  ([rb ont]
    (reify ArgumentGenerator
      (generate [subgoal subs]
                (letfn [(apply-for-conclusion
                          [clause c]
                          ;; apply the clause for conclusion
                          ;; in the head of the rule
                          (let [subs2 (or (unify c subgoal subs)
                                          (unify `(~'unless ~c)
                                                 subgoal subs)
                                          (unify `(~'assuming ~c)
                                                 subgoal subs)
                                          (unify `(~'applies ~(:rule clause) ~c) subgoal subs))]
                            (if (not subs2)
                              false ; fail
                              
        ;  (let [inst-clauses (instantiate-domains clause subs2)]
        ;                     (map (fn [inst-clause-map]
        ;                             (let [ic (:clause inst-clause-map)]
        ;                               (make-response (:subs inst-clause-map)
        ;                                         (clause-assumptions (:clause clause))
        ;                                         (argument (gensym "a")
        ;                                                   false
        ;                                                   *default-weight*
        ;                                                   (if (= (first subgoal) 'not) :con :pro)
        ;                                                   (statement-atom (condition-statement subgoal))
        ;                                                   (map condition->premise (:clause ic))
        ;                                                  (str (:rule ic))))))
        ;                          inst-clauses)))))
                              
                              
                              (list (make-response subs2
                                                   (clause-assumptions (:clause clause))
                                                   (make-argument 
                                                     :id (gensym "a")
                                                     :conclusion (condition-statement subgoal)
                                                     :premises (map condition->premise (:clause clause))
                                                     :scheme (str (:rule clause))))))))
                        
                        (apply-clause [clause]
                                      (apply concat (filter identity 
                                                            (map #(apply-for-conclusion clause %) 
                                                                 (:head clause)))))]
                  (mapinterleave
                    (fn [c] (apply-clause c))
                    (map rename-clause-variables (get-clauses rb subgoal subs))))))))

