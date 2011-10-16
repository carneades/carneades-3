(ns 
  ^{:doc "Defines the structure to be returned by argument generators.
          An argument generator is a function of the type:
          
          statement substitutions -> (seq-of response)
          
          The arguments returned by a generator need not be fully
          instantiated by the substitutions. The argument construction
          module will create goals for the premises of the argument
          which are not ground."}
  
  carneades.engine.response)

; type generator: statement substitutions -> (seq-of response)

(defrecord response
  [substitutions   ; (term -> term) map
   assumptions     ; set of literals
   argument])      ; argument | nil

(defn make-response [subs asms arg] (response. subs asms arg))