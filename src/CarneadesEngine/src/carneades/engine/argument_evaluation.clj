(ns ^{:doc "The protocol for argument evaluation structures."}
  carneades.engine.argument-evaluation
  (:use carneades.engine.statement
        carneades.engine.argument-graph))

; type label = integer in the range of 0.0 ... 1.0
; where 1.0 means "in", 0.0 means "out" and anything in between 
; means "undecided".  (Thanks to Trevor for this idea.)

(defrecord Evaluation 
  [statement-labels   ; (symbol -> label) map, where the keys are statement node ids
   argument-labels])  ; (symbol -> label) map, where the keys are argument node ids

(defn make-evaluation 
  [statement-labels argument-labels]
  (ArgumentEvaluation. statement-labels argument-labels))

(defprotocol ArgumentEvaluator
  "Protocol for argument evaluation structures."
  (evaluate [ag] "argument-graph -> evaluation"))


