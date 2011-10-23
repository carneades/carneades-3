(ns ^{:doc "The protocol for argument evaluation structures."}
  carneades.engine.argument-evaluation
  (:use carneades.engine.statement
        carneades.engine.argument-graph))

(defn label?
  "The set of argument and statement labels is {:in, :out, :undecided}."
  [x]
  (contains? #{:in, :out, :undecided} x))

(defrecord Evaluation 
  [statement-labels   ; (symbol -> label) map, where the keys are statement node ids
   argument-labels])  ; (symbol -> label) map, where the keys are argument node ids

(defn make-evaluation 
  [statement-labels argument-labels]
  (ArgumentEvaluation. statement-labels argument-labels))

(defprotocol ArgumentEvaluator
  "Protocol for argument evaluation structures."
  (evaluate [ag] "argument-graph -> evaluation"))


