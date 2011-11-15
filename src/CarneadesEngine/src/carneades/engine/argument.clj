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
  [literal     ; literal
   role        ; string; the role of the premise in the scheme used to create this argument
   implicit])  ; boolean; true if the premise was not explicit in the source document.

(defn premise? [x] (instance? Premise x))

(defn make-premise 
  "key value ... -> premise"
  [& key-values]  
  (merge (Premise. 
           nil           ; literal
           ""            ; role
           false)        ; implicit
         (apply hash-map key-values)))

(defn pm
  "literal -> premise"
  [literal]
  (make-premise :literal literal))

(defrecord Argument
  [id               ; symbol
   header           ; nil or dublin core metadata about the argument
   scheme           ; nil or symbol, for the scheme id
   strict           ; boolean
   weight           ; real number between 0.0 and 1.0, default 0.5
   conclusion       ; nil or literal
   premises         ; sequence of premises 
   sources])        ; collection of dublin-core metadata structures

(defn argument? [x] (instance? Argument x))

(defn argument-variables
  "arg -> (seq-of symbol)
   Returns a seq containing the variables of the argument"
  [arg]
  (distinct (concat (mapcat (fn [p] (variables (:literal p)))
                            (:premises arg))
                    (variables (:conclusion arg)))))

(defn make-argument
  "Makes a one-step argument."
  [& values]
  (-> (Argument. 
        (gensym "a") ; id
        nil          ; header
        nil          ; scheme
        false        ; strict
        0.5          ; weight
        nil          ; conclusion
        []           ; premises
        [])          ; sources  
      (merge (apply hash-map values))))

(defn instantiate-argument
  "argument substitutions -> arg
   Instantiate the variables of an argument by applying substitions"
  [arg subs]
  (assoc arg
         :id (gensym "a")
         :premises (map (fn [p] (assoc p 
                                       :literal 
                                       (apply-substitutions subs (:literal p)))) 
                        (:premises arg))
         :conclusion (apply-substitutions subs (:conclusion arg))))


