;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Theories defined using argumentation schemes."}
  carneades.engine.theory
  (:use clojure.pprint
        [clojure.set :as set]
        carneades.engine.uuid
        carneades.engine.statement
        carneades.engine.unify
        carneades.engine.argument
        carneades.engine.argument-generator
        carneades.engine.dublin-core
        [carneades.engine.utils :only (mapinterleave)])
  (:require [carneades.engine.theory.namespace :as namespace]))

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

(defprotocol Functor
  (get-symbol [this])
  (get-arity [this]))

(defrecord Individual
    [symbol
     text]
  Functor
  (get-arity [this] 0)
  (get-symbol [this] (:symbol this)))

(defn make-individual
  "key value ... -> individual"
  [& key-values]
  {:post [(instance? Individual %)]}
  (merge (Individual.
          (gensym "i")    ; symbol
          {})             ; text map
         (apply hash-map key-values)))

(defn individual? [x] (instance? Individual x))

(defrecord Function
    [symbol  ;; symbol
     arity   ;; integer
     text]   ;; (lang -> string) map
  Functor
  (get-symbol [this] (:symbol this))
  (get-arity [this] (:arity this)))

(defn make-function
  "key value ... -> function"
  [& key-values]
  (merge (Function.
          (gensym "f")    ; symbol
          0               ; arity
          {})             ; text map
         (apply hash-map key-values)))

(defn function? [x] (instance? Function x))

(defrecord Predicate
    [symbol   ; symbol
     arity    ; integer
     askable  ; boolean
     forms    ; (lang -> form) map, where lang is one of the keywords :en, :de, etc.
     category
     hint     ; lang -> string map
     widgets
     followups]
  Functor
  (get-symbol [this] (:symbol this))
  (get-arity [this] (:arity this)))

(defn make-predicate
  "key value ... -> predicate"
  [& key-values]
  (merge (Predicate.
          (gensym "p")                 ; symbol
          0                            ; arity
          false                        ; askable
          {}                           ; forms
          nil                          ; category
          {}                           ; hint
          nil                          ; widgets
          []                           ; followups
          )
         (apply hash-map key-values)))

(defn predicate? [x] (instance? Predicate x))

;; Concepts are unary relations, as in description logic and the
;; semantic web, where they are called "classes".  That is, the
;; represent atoms of the form (predicate object).

(defrecord Concept
    [symbol       ; predicate symbol
     category     ; symbol
     askable      ; boolean
     hint         ; lang -> string map
     followups]  ; vector of predicate symbols
  Functor
  (get-symbol [this] (:symbol this))
  (get-arity [this] 1))


(defn make-concept
  "key value ... -> class"
  [& key-values]
  (merge (Concept.
          (gensym "c")    ; symbol
          nil             ; category
          false           ; askable
          {}              ; hint
          [])             ; followups
         (apply hash-map key-values)))

(defn concept? [x] (instance? Concept x))

;; Roles are binary relations, as in description logic and the
;; semantic web, where they are called "properties".  That is, the
;; represent triples of the form (predicate object value).  Use
;; roles to ask users for information. The predicate and object
;; fields of the triple must be instantiated. Only the values are
;; asked for, or confirmed, in dialogues with users. By restricting
;; data entry dialogues to properties, rather that arbitrary
;; predicates, the dialogue can be structured in a more coherent and
;; user-friendly way.

(defrecord Role
    [symbol        ; predicate symbol
     min           ; minimum cardinality; whole numberp
     max           ; maximum cardinality; whole number or nil, for unlimited
     type          ; :symbol (object id), :boolean, :string, :uri, :date-time, :integer, :real,
                                        ; '(enum object object ...)
     askable       ; boolean
     default       ; element of the above type or nil
     forms         ; Do we need the negated and question forms, or just the positive?
     category
     hint          ; lang -> string map
     followups]
  Functor
  (get-symbol [this] (:symbol this))
  (get-arity [this] 2))


(defn make-role
  "key value ... -> role"
  [& key-values]
  (merge (Role.
          (gensym "p")                 ; symbol
          1                            ; min
          1                            ; max
          :string                      ; type
          false                        ; askable
          ""                           ; default
          {}                           ; forms
          ""                           ; category
          ""                           ; hint
          []                           ; followups
          )
         (apply hash-map key-values)))

(defn role? [x] (instance? Role x))

(defn functional-role?
  [x]
  {:pre [(role? x)]}
  (and (= (:min x) 1)
       (= (:max x) 1)))

(defn replace-role-obj
  "Changes the object in the role by a new object."
  [role newobj]
  (let [[p s o] role]
    (list p s newobj)))

(defn make-language
  "functor ... -> map"
  [& functors]
  (zipmap (map get-symbol functors) functors))

;; variables are allowed as conclusions of schemes to enable them
;; to represent argumentation schemes, such as arguments from
;; expert witness testimony, whose conclusions can be any (literal) proposition.

(defrecord Scheme
    [id            ; symbol
     header        ; nil or dublin metadata
     conclusion    ; atom or variable (ranging over literals)
     pro           ; boolean; generates con arguments if false
     strict        ; boolean, defeasible if false
     weight        ; nil or number in the range 0.0 to 1.0
     premises      ; premise sequence
     exceptions    ; premise sequence
     assumptions]) ; premise sequence

                                        ; The scope of scheme ids is local to their section in the theory.

(defn make-scheme
  "key value ... -> scheme"
  [& key-values]
  (let [m  (-> (merge (Scheme.
                       (gensym "s")    ; id
                       nil             ; header
                       nil             ; conclusion
                       true            ; pro
                       false           ; strict
                       0.5             ; weight
                       []              ; premises
                       []              ; exceptions
                       [])             ; assumptions
                      (apply hash-map key-values)))]
                                        ; normalize the conclusion and direction of the scheme
    (assoc m :conclusion (literal-atom (:conclusion m))
           :pro  (or (and (literal-pos? (:conclusion m))
                          (:pro m))
                     (and (literal-neg? (:conclusion m))
                          (not (:pro m)))))))

(defn scheme? [x] (instance? Scheme x))

(defn scheme-conclusion-literal
  "scheme -> literal
   Returns the conclusion of the scheme as a positive
   literal, if the scheme is pro, or negative literal,
   if the scheme gument is con."
  [s]
  {:pre [(scheme? s)]}
  (if (:pro s)
    (:conclusion s)
    (literal-complement (:conclusion s))))

(defn scheme-variables
  "scheme -> (seq-of symbol)"
  [scheme]
  (distinct (mapcat variables
                    (concat [(:conclusion scheme)]
                            (:premises scheme)
                            (:exceptions scheme)
                            (:assumptions scheme)))))

(defn specialize-scheme
  "scheme substitutions -> scheme
   Instantiate or partially instantiate a scheme by substituting
   variables in the scheme with their values in the map."
  [scheme subs]
  {:pre [(scheme? scheme) (map? subs)]}
  (letfn [(apply-subs [literal] (apply-substitutions subs literal))]
    (assoc scheme
      :conclusion (apply-subs (:conclusion scheme)),
      :premises (map (fn [p] (assoc p :statement (apply-subs (:statement p))))
                     (:premises scheme)),
      :exceptions (map (fn [p] (assoc p :statement (apply-subs (:statement p))))
                       (:exceptions scheme)),
      :assumptions (map (fn [p] (assoc p :statement (apply-subs (:statement p))))
                        (:assumptions scheme)))))

(defn generate-exceptions
  "Generates the arguments representing the exceptions"
  [arg]
  (map (fn [e] (make-response
                subs
                []
                (make-argument
                 :id (make-urn-symbol)
                 :conclusion (list 'undercut (symbol (:id arg)))
                 :pro true
                 :strict false
                 :weight 0.5
                 :premises [e]
                 :exceptions []
                 :scheme (:scheme arg))))
       (:exceptions arg)))

(defn instantiate-scheme
  "scheme map -> (seq-of response)
   Constructs a sequence of grounds arguments, in responses, by
   substituting variables in the scheme with their values in a map of
   substitutions. Only ground arguments are included in the result. It
   is the responsibility of the caller to provide substitutions for all
   variables in the argument.  The first response has the main argument;
   there is a further response with an undercutter for each exception of
   the scheme."
  [scheme subs]
  (let [conclusion (apply-substitutions subs (:conclusion scheme))
        main-arg (instantiate-argument
                  (make-argument
                   :conclusion (literal-atom conclusion),
                   :pro (literal-pos? conclusion)
                   :strict (:strict scheme),
                   :weight (:weight scheme),
                   :premises (map (fn [p] (apply-substitutions subs p))
                                  (concat (:premises scheme) (:assumptions scheme))),
                   :exceptions (map (fn [p] (apply-substitutions subs p))
                                    (:exceptions scheme)),
                   :scheme `(~(:id scheme) ~@(scheme-variables scheme)))
                  subs)]
    (if (not (ground-argument? main-arg))
      ()
      ;; cons
      [(make-response
        subs
        (map (fn [p] (if (:positive p)
                       (:statement p)
                       (literal-complement (:statement p))))
             (:assumptions scheme))
        main-arg)])))


(defn axiom
  "literal -> scheme"
  [literal]
  (make-scheme
   :strict true
   :conclusion (literal-atom literal)
   :pro (literal-pos? literal)))

;; The scope of section ids is their theory

(defrecord Section
    [id          ; symbol
     header      ; nil or a dublin core metadata structure about this model
     schemes     ; sequence of schemes
     sections])  ; sequence of sections; i.e. subsections

(defn make-section
  "key value ... -> section"
  [& key-values]
  (merge (Section.
          (gensym "sect-")    ; id
          nil             ; header
          []              ; schemes
          [])             ; (sub)sections
         (apply hash-map key-values)))

(defn section? [x] (instance? Section x))

(defn- rename-variables-in-premises
  "substitutions (seq-of premise) -> [substitutions (seq-of premise)]"
  [subs premises]
  (reduce (fn [pair p]
            (let [[subs1 atom] (rename-variables (first pair) (:statement p))]
              [subs1 (conj (second pair) (assoc p :statement atom))]))
          [subs []]
          premises))

(defn- rename-scheme-variables
  [scheme]
  ;; {:post [(do (prn "=>") (pprint %) true)]}
  ;; (prn "scheme =")
  ;; (pprint scheme)
  (let [[m1 conclusion] (rename-variables {} (:conclusion scheme)),
        [m2 premises] (rename-variables-in-premises m1 (:premises scheme)),
        [m3 exceptions] (rename-variables-in-premises m2 (:exceptions scheme)),
        [m4 assumptions] (rename-variables-in-premises m3 (:assumptions scheme))]
    (assoc scheme
      :conclusion conclusion
      :premises premises
      :exceptions exceptions
      :assumptions assumptions)))

(defrecord Theory
    [header     ; nil or a dublin core metadata structure about the model.
     namespaces ; (string -> string) map
     language   ; (symbol -> individual or predicate) map
     schemes    ; scheme sequence
     sections   ; section sequence
     references]) ; (string to metadata) map

(defn imports-import
  [theory import]
  (update-in theory [:sections] concat (:sections import)))

(defn imports-imports
  "Adds the imports of the theory to the theory."
  [theory]
  (let [theory (reduce imports-import theory (:imports theory))
        theory (dissoc theory :imports)]
    theory))

(defn make-theory
  "key value ... -> theory"
  [& key-values]
  (-> (Theory.
       nil                          ; header
       {}                           ; namespaces
       {}                           ; language
       []                           ; schemes
       []                           ; sections
       {})
      (merge ,,, (apply hash-map key-values))
      (namespace/to-absolute-theory ,,,)
      (imports-imports ,,,)))

(defn theory? [x] (instance? Theory x))

(defn load-theory
  "Dynamically loads the theory at url and returns it."
  [filepath]
  (deref (load-file filepath)))

;; ensure we don't load twice the same namespace
(memoize load-theory)

(defn- scheme-index-key
  "term -> symbol
   Returns the symbol used to index a scheme by its conclusions
   for quicker retrieval. By default schemes are indexed under :other."
  [trm]
  {:pre [(term? trm)]}
                                        ; (println "term: " trm)
  (cond (constant? trm) trm,
        (variable? trm) :other,
        (literal? trm) (literal-predicate trm),
        (compound-term? trm) (term-functor trm)
        :else :other))

(defn create-scheme-predicate-index
  "map section-or-theory -> (symbol -> seq of schemes) map
   map from predicate symbols to scheme sequences"
  [map1 part]
                                        ; (println "section: " section)
  (let [map2 (reduce (fn [map2 scheme]
                                        ; (println "scheme: " scheme)
                       (assoc map2
                         (scheme-index-key (:conclusion scheme))
                         (conj (get map2 (scheme-index-key (:conclusion scheme)))
                               scheme)))
                     map1
                     (:schemes part))]
    (reduce (fn [map3 section]
              (create-scheme-predicate-index map3 section))
            map2
            (:sections part))))

(defn create-scheme-id-index
  "map section-or-theory -> (symbol -> scheme) map
   map from scheme ids to schemes. Assumes that scheme ids are unique."
  [map1 part]
                                        ; (println "section: " section)
  (let [map2 (reduce (fn [map2 scheme]
                                        ; (println "scheme: " scheme)
                       (assoc map2 (:id scheme) scheme))
                     map1
                     (:schemes part))]
    (reduce (fn [map3 section]
              (create-scheme-id-index map3 section))
            map2
            (:sections part))))

(defn get-schemes
  "map goal substititions boolean -> sequence of schemes
   where the goal is a literal. If 'other' is false,
   does not return :other schemes, which match every goal
   and are not well controlled when using with backward
   chaining."
  ([index goal subs]
     (get-schemes index goal subs false))
  ([index goal subs other]
     {:pre [(map? index)
            (literal? goal)
            (map? subs)]}
     (let [key (scheme-index-key (apply-substitutions subs goal))]
       (if (and (not other) (= key :other))
         ()
         (or (get index key) [])))))

(defn apply-scheme
  "scheme literal substitutions -> seq-of response"
  [scheme goal subs]
  {:pre [(scheme? scheme) (literal? goal) (map? subs)]}
  (let [c (scheme-conclusion-literal scheme)
        subs2 (unify c goal subs)]
    (if (not subs2)
      [] ; fail
      (let [id (make-urn-symbol)]
        [(make-response subs2
                        (map (fn [p] (if (:positive p)
                                       (:statement p)
                                       (literal-complement (:statement p))))
                             (:assumptions scheme))
                        (make-argument
                         :id id,
                         :conclusion (:conclusion scheme),
                         :pro (literal-pos? goal),
                         :strict (:strict scheme),
                         :weight (:weight scheme),
                         :premises (concat (:premises scheme)
                                           (:assumptions scheme)),
                         :exceptions (:exceptions scheme),
                         :scheme `(~(:id scheme)
                                   ~@(apply-substitutions subs2 (scheme-variables scheme)))))]))))

;; Generators for arguments from schemes and theories:

(defn generate-arguments-from-scheme
  "scheme -> argument-generator"
  [scheme]
  (reify ArgumentGenerator
    (generate [this goal subs]
      (apply-scheme scheme goal subs))))

(defn generate-arguments-from-theory
  "theory -> argument-generator"
  [theory1]
  (let [index (create-scheme-predicate-index {} theory1)]
    (reify ArgumentGenerator
      (generate [this goal subs]
        (mapinterleave
         (fn [s] (apply-scheme s goal subs))
         (map rename-scheme-variables (get-schemes index goal subs)))))))
