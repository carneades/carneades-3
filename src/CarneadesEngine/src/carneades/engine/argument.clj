;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns 
  ^{:doc "Provides a data structure for representing one-step arguments.
          The premises and conclusion of an argument are statements,
          representing propositional or predicate logic literals.
          An argument need not be fully instantiated; the premises
          and conclusion of the argument may contain free variables."}
  
  carneades.engine.argument
  (:use carneades.engine.statement
        carneades.engine.dublin-core
        carneades.engine.unify))

(defrecord Premise
  [statement   ; atomic statement
   positive    ; boolean
   role        ; string; the role of the premise in the scheme used to create this argument
   implicit])  ; boolean; true if the premise was not explicit in the source document.

(defn premise? [x] (instance? Premise x))

(defn make-premise 
  "key value ... -> premise"
  [& key-values]  
  (let [m (merge (Premise. 
                   nil           ; atomic statement
                   true          ; positive
                   ""            ; role
                   false)        ; implicit
                 (apply hash-map key-values))]
    ; normalize the statement of the premise:
    (assoc m :statement (literal-atom (:statement m))
           :positive  (or (and (literal-pos? (:statement m))
                               (:positive m))
                          (and (literal-neg? (:statement m))
                               (not (:positive m)))))))

(defn pm
  "literal -> premise"
  [literal]
  (make-premise :statement literal))

(defn premise-literal
  "premise -> literal"
  [premise]
  (if (:positive premise) 
    (:statement premise)
    (literal-complement (:statement premise))))

(defrecord Argument
  [id               ; symbol
   header           ; nil or dublin core metadata about the argument
   scheme           ; nil, symbol or string, the URI of the scheme
   strict           ; boolean
   weight           ; real number between 0.0 and 1.0, default 0.5
   conclusion       ; literal
   pro              ; boolean; con argument if false
   premises])       ; sequence of premises 

(defn argument? [x] (instance? Argument x))

(defn argument-variables
  "arg -> (seq-of symbol)
   Returns a seq containing the variables of the argument"
  [arg]
  (distinct (concat (mapcat (fn [p] (variables (:statement p)))
                            (:premises arg))
                    (variables (:conclusion arg)))))


(defn map->argument 
  "Makes a one-step argument."
  [m]
  (let [m2 (merge  (Argument. 
                     (gensym "a") ; id
                     nil          ; header
                     nil          ; scheme
                     false        ; strict
                     0.5          ; weight
                     nil          ; conclusion
                     true         ; pro
                     [])          ; premises 
                  m)]
    ; normalize the conclusion and direction of the argument:
    (assoc m2 
           :conclusion (literal-atom (:conclusion m2))
           :pro  (or (and (literal-pos? (:conclusion m2))
                          (:pro m2))
                     (and (literal-neg? (:conclusion m2))
                          (not (:pro m2)))))))

(defn make-argument [& values]
  (map->argument (apply hash-map values)))

(defn conclusion-literal
  "argument -> literal
   Returns the conclusion of the argument as a positive 
   literal, if the argument is pro, or negative literal,
   if the argument is con."
  [arg]
  {:pre [(argument? arg)]}
  (if (:pro arg) 
    (:conclusion arg) 
    (literal-complement (:conclusion arg))))

(defn instantiate-argument
  "argument substitutions -> arg
   Instantiate the variables of an argument by applying substitions"
  [arg subs]
  (assoc arg
         :id (gensym "a")
         :premises (map (fn [p] (assoc p 
                                       :statement 
                                       (apply-substitutions subs (:statement p)))) 
                        (:premises arg))
         :conclusion (apply-substitutions subs (:conclusion arg))))


