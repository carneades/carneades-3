;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns 
  ^{:doc "Provides a data structure for representing one-step arguments.
          The premises and conclusion of an argument are statements,
          representing propositional or predicate logic literals.
          An argument need not be fully instantiated; the premises
          and conclusion of the argument may contain free variables."}
  
  carneades.engine.argument
  (:use carneades.engine.uuid
        carneades.engine.statement
        carneades.engine.dublin-core
        carneades.engine.unify
        carneades.engine.utils))

(defrecord Premise
  [statement   ; positive literal
   positive    ; boolean
   role        ; string; the role of the premise in the scheme used to create this argument
   implicit])  ; boolean; true if the premise was not explicit in the source document.

(defn premise? [x] (instance? Premise x))

(defn map->premise
  "Makes a premise"
  [m]
  ;; {:pre [(not (contains? m :pro))
  ;;        (not (contains? m :con))]}
  (let [m2 (merge (Premise. 
                    nil    ; statement
                    true   ; positive
                    ""     ; role
                    false) ; implicit
                  m)]
    ; normalize the premise
    (assoc m2
           :statement (positive-statement (:statement m2))
           :positive (or (and (literal-pos? (:statement m2))
                               (:positive m2))
                          (and (literal-neg? (:statement m2))
                                (not (:positive m2)))))))
  
(defn make-premise [& key-values]
  (map->premise (apply hash-map key-values)))

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
   scheme           ; nil or (symbol term ...) form, where the symbol
   ;; is the URI of the scheme and the terms are variables or constants
   ;; which instantiate the variables of the scheme
   strict           ; boolean
   weight           ; real number between 0.0 and 1.0, default 0.5
   value            ; nil or real number between 0.0 and 1.0, default nil
   conclusion       ; literal
   pro              ; boolean; con argument if false
   premises         ; sequence of premises
   exceptions])     ; sequence of premises

(defn argument? [x] (instance? Argument x))

(defn argument-variables
  "arg -> (seq-of symbol)
   Returns a seq containing the variables of the argument"
  [arg]
  (distinct (concat (mapcat (fn [p] (variables (:statement p)))
                            (concat (:premises arg) (:exceptions arg)))
                    (variables (:conclusion arg)))))

(defn ground-argument? [arg]
  {:pre [(argument? arg)]
   :post [(instance? java.lang.Boolean %)]}
  (-> arg (argument-variables) (empty?)))

(defn map->argument 
  "Makes a one-step argument."
  [m]
  (let [m2 (merge  (Argument. 
                     nil          ; id
                     nil          ; header
                     nil          ; scheme
                     false        ; strict
                     0.5          ; weight
                     nil          ; value
                     nil          ; conclusion
                     true         ; pro
                     []           ; premises
                     [])          ; exceptions
                  m)]
    ; normalize the conclusion and direction of the arguments
    ; and assign the argument an id if needed
    (assoc m2 
           :id (if (:id m) (:id m) (make-urn-symbol))
           :conclusion (if (literal-pos? (:conclusion m2)) 
                          (:conclusion m2)  
                          (literal-complement (:conclusion m2)))
           :pro  (or (and (literal-pos? (:conclusion m2))
                          (:pro m2))
                     (and (literal-neg? (:conclusion m2))
                          (not (:pro m2)))))))

(defn make-argument [& key-values]
  {:post [(not (string? (:scheme %)))]}
  (map->argument (apply hash-map key-values)))

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
   Instantiate the variables of an argument by applying substitions.
   The resulting argument need not be fully instantiated."
  [arg subs]
  (let [update-statement
        (fn [p]
          (let [statement (:statement p)
                statement (apply-substitutions subs statement)
                statement (assoc statement :id (make-urn-symbol))]
            (assoc p :statement statement)))]
   (assoc arg
     :id (make-urn-symbol)
     :scheme (apply-substitutions subs (:scheme arg))
     :premises (map update-statement (:premises arg))
     :exceptions (map update-statement (:exceptions arg))
     :conclusion (apply-substitutions subs (:conclusion arg)))))

(defn make-undercutters
  "argument -> seq-of argument
   Returns an undercutter for each exception of the argument"
  [arg]
  (map (fn [e]
         (make-argument
          :id (make-urn-symbol)
          :scheme (safe-read-string (:role e))
          :conclusion `(~'undercut ~(:id arg))
          :premises [e]))
       (:exceptions arg)))
  


