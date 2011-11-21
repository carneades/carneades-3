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
  (merge (Premise. 
           nil           ; atomic statement
           true          ; positive
           ""            ; role
           false)        ; implicit
         (apply hash-map key-values)))

(defn pm
  "literal -> premise"
  [literal]
  (make-premise :statement (literal-atom literal)
                :positive (literal-pos? literal)))

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

(defn make-argument
  "Makes a one-step argument."
  [& values]
  (let [m (apply hash-map values)]
    (-> (Argument. 
          (gensym "a") ; id
          nil          ; header
          nil          ; scheme
          false        ; strict
          0.5          ; weight
          nil          ; conclusion
          true         ; pro
          [])          ; premises 
        (merge m))))

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


